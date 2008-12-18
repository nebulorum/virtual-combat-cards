//$Id$
package vcc.view.actor

case class Combatant(vc:ViewCombatant)
case class SetHealth(id:Symbol, hts:vcc.model.HealthTrackerSummary)
case class SetInitiative(id:Symbol, init:vcc.model.InitiativeTracker)
case class SetSequence(seq:Seq[Symbol])
case class SetInformation(id:Symbol,text:String)
case class SetContext(id:Symbol)
case class GoToFirst() 

import scala.actors.Actor._
import scala.actors.Actor

class UserInterface(tracker:Actor) extends Actor {
  
  type T=ViewCombatant
  
  private var _ctx:Option[T]=None
  private var seqAware:List[SequenceView[T]]=Nil
  private var ctxAware:List[ContextualView[T]]=Nil
  private val _map=scala.collection.mutable.Map.empty[Symbol,T]
  object InMap {
    def unapply(id:Symbol):Option[T] = if(id!=null && _map.contains(id)) Some(_map(id)) else None
  }
  
  protected def signalContext(o:Option[T]) {for(x<-ctxAware) x.context=(o)}
  protected def signalSequence(seq:Seq[T]) {for(x<-seqAware) x.updateSequence(seq)}
  def flush():Unit = {
    _map.clear
    signalContext(None)
    signalSequence(Nil)
  }
  
  private var _first:ViewCombatant=null;
  
  def addSequenceListener(seq:SequenceView[T]) { seqAware=seq :: seqAware }
  def addContextListner(ctx:ContextualView[T]) { ctxAware=ctx :: ctxAware }
  
  def act() {
    loop {
      react {
        case SetContext(null) => _ctx=None; signalContext(None);
        case SetContext(InMap(o)) => _ctx=Some(o); signalContext(_ctx);
        case Combatant(c) => _map(c.id)=c
        case SetHealth(InMap(o), hts)=> 
          o.health=hts
          if(_ctx == Some(o)) signalContext(Some(o))
        case SetInitiative(InMap(o), inits)=>
          o.initTracker=inits
          if(_ctx == Some(o)) signalContext(Some(o))
        case SetInformation(InMap(o),text)=>
          o.info=text
          if(_ctx == Some(o)) signalContext(Some(o))
        case SetSequence(seq)=>
          //for(x<-_map) println(x)
          var l=seq.filter(_map.contains(_)).map(_map(_))
          _first=if(l.isEmpty) null else l(0)
          signalSequence(l)
        case GoToFirst() =>
          _ctx=Some(_first)
          signalContext(_ctx)
        //case s => println("UserInterface: "+s)
      }
    }
  }
  def sequence():Seq[ViewCombatant]=_map.map(x=>x._2).toSeq
}
