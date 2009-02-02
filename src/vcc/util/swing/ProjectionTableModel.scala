//$Id$
package vcc.util.swing

abstract class TableModelRowProjection[T] {
  val columns:List[(String,java.lang.Class[_])]
  def size = columns.size
  val setter:PartialFunction[(Int,T,Any),Unit]
  def getColumnClass(col:Int):java.lang.Class[_]=columns(col)._2
  def getColumnName(col:Int):String=columns(col)._1
  def isEditable(col:Int,obj:T)=if(setter!=null)setter.isDefinedAt(col,obj,null) else false
  def apply(col:Int,obj:T):java.lang.Object
  def set(col:Int,obj:T,value:Any):Unit=if(setter!=null && setter.isDefinedAt(col,obj,value)) setter(col,obj,value)
}

object SwingHelper {
  
  def makeRunnable(f:()=>Unit):java.lang.Runnable =
    new java.lang.Runnable {
      def run() {
        f.apply()
      }
    } 
  
  def invokeLater(f:()=>Unit) {
    javax.swing.SwingUtilities.invokeLater(makeRunnable(f))
  }
}

class ProjectionTableModel[T](val proj:TableModelRowProjection[T]) extends javax.swing.table.AbstractTableModel {
  var elem:Seq[T]=Nil
  def getValueAt(row:Int,col:Int):java.lang.Object= if(elem.size>row )proj(col,elem(row)) else null
  def getColumnCount():Int = proj.size
  def getRowCount():Int=elem.size
  override def getColumnName(col:Int)=proj.getColumnName(col)
  override def getColumnClass(col:Int)=proj.getColumnClass(col)
  override def isCellEditable(row:Int,col:Int)=(elem.size>row) && proj.isEditable(col,elem(row))
  override def setValueAt(value:java.lang.Object,row:Int,col:Int)=proj.set(col,elem(row),value)
  
  def content_=(content:Seq[T])={
    SwingHelper.invokeLater {
      var s=elem.size
      elem=content;
      if(s>content.size) 
        this.fireTableRowsDeleted(content.size,s)
      this.fireTableDataChanged; 
    }
  }
  def content=elem
}
