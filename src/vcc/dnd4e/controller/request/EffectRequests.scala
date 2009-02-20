//$Id$
package vcc.dnd4e.controller.request

import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model.Effect

case class AddEffect(to:Symbol,effect:Effect) extends TransactionalAction {
  def description = "Add effect "+effect+ " to "+to
}

case class CancelEffect(to:Symbol,pos:Int) extends TransactionalAction {
  def description = "Cancel effect number "+pos + " of "+to
}