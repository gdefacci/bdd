package org.obl.bdd

case class FilePosition(path:String, lineNumber:Int) {
  
  override def toString = s"line $lineNumber in file $path"
  
}