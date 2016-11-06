package com.github.gdefacci.bdd

case class FilePosition(path:String, lineNumber:Int) {
  
  override def toString = s"line $lineNumber in file $path"
  
}