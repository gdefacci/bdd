package org.obl.bdd

sealed trait Description {
  
  def mkString(sep:String):String = Description.getTexts(this).mkString(sep)
  
  def add(d:Description) = this match {
    case t1 @ Text(_,_) => Descriptions(Seq(t1, d))
    case Descriptions(descs) => Descriptions(descs :+ d)
  }
  
  def append(conjuction:Conjuction):Description
}

object Description {
  private def getTexts(d:Description):Seq[String] = d match {
    case Text(str, conj) => (str + conj.map( c => " "+c.text).getOrElse("")) :: Nil
    case Descriptions(descs) => descs.flatMap(getTexts(_))
  }
}

case class Text(value:String, conjuction:Option[Conjuction] = None) extends Description {
  def append(conjuction:Conjuction) = copy(conjuction = Some(conjuction))
}
case class Descriptions(values:Seq[Description]) extends Description {
  def append(conjuction:Conjuction):Description = Descriptions(values.init :+ values.last.append(conjuction))
}

sealed case class Conjuction(text:String)

object Conjuction {
  
  object And extends Conjuction("and")
  object But extends Conjuction("but")
  object Then extends Conjuction("then")
  object When extends Conjuction("when")
  object + extends Conjuction("+")
  
}
