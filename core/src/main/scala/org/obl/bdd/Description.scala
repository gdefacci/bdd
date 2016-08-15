package org.obl.bdd

sealed trait Description {
  
  def mkString(sep:String):String = Description.getTexts(this).mkString(sep)
  
  def add(d:Description) = this match {
    case t1 @ Text(_,_) => Descriptions(Seq(t1, d))
    case Descriptions(descs) => Descriptions(descs :+ d)
  }
  
  def prepend(conjuction:Conjuction):Description
}

object Description {
  private def getTexts(d:Description):Seq[String] = d match {
    case Text(conj, str) => (conj.map( c => c.text+" ").getOrElse("") + str) :: Nil
    case Descriptions(descs) => descs.flatMap(getTexts(_))
  }
}

object Text {
  def apply(value:String) = new Text(None, value)
}

case class Text(conjuction:Option[Conjuction], value:String) extends Description {
  
  def prepend(conjuction:Conjuction) = copy(conjuction = Some(conjuction))
}
case class Descriptions(values:Seq[Description]) extends Description {
  def prepend(conjuction:Conjuction):Description = Descriptions(values.head.prepend(conjuction) +: values.tail)
}

sealed case class Conjuction(text:String)

object Conjuction {
  
  object And extends Conjuction("and")
  object But extends Conjuction("but")
  object Then extends Conjuction("then")
  object When extends Conjuction("when")
  object - extends Conjuction("-")

  object Or extends Conjuction("or")
  
}
