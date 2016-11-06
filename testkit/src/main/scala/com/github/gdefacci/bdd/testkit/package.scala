package com.github.gdefacci.bdd

import scalaz.Monad
import scalaz.Traverse
import scala.util.Try
import scalaz.Applicative
import scala.util.Failure
import scala.util.Success

import language.higherKinds

package object testkit {
  implicit val tryMonad = new Monad[Try] with Traverse[Try] {

    def point[A](a: => A): scala.util.Try[A] = Try(a)
    def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    def traverseImpl[G[_], A, B](fa: Try[A])(f: A => G[B])(implicit app: Applicative[G]): G[Try[B]] = fa match {
      case Failure(x) => Applicative[G].point(Failure(x))
      case Success(x) => Applicative[G].map(f(x))(Success(_))
    }
  }
}