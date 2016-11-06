package org.obl.bdd

class Predicate[-A](val description: Description, predicate: A => Boolean, val filePosition: Option[FilePosition]) extends (A => Boolean) {

  def apply(a: A): Boolean = predicate(a)
  override def toString = description.mkString(" ")

  def not = new Predicate[A](description.prepend(Conjuction.Not), a => !apply(a), filePosition)
}

object Predicate {

  implicit class PredicateBuilder[A](self: Predicate[A]) {
    
    def and(predicate: Predicate[A]): Predicate[A] = and(predicate, Conjuction.And)
    def but(predicate: Predicate[A]): Predicate[A] = and(predicate, Conjuction.But)

    private def and(predicate: Predicate[A], conj: Conjuction) =
      new Predicate[A](self.description.add(predicate.description.prepend(conj)), a => self.apply(a) && predicate(a), self.filePosition)

    def or(predicate: Predicate[A]) =
      new Predicate[A](self.description.add(predicate.description.prepend(Conjuction.Or)), a => self.apply(a) || predicate(a), self.filePosition)
  }

}