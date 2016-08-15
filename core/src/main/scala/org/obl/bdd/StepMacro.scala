package org.obl.bdd

import scala.reflect.macros.blackbox.Context

object StepMacro {

  def symbolLiftable(c:Context) = c.universe.Liftable[c.Symbol] { sym =>
    import c.universe._
    q"${sym.asTerm.name}"
  }
  
  def newDescriptionAndParameterType(c:Context)(typ:c.Tree, f:c.Tree) = {
    import c.universe._
    
    val params = c.internal.enclosingOwner.asMethod.paramLists
    
    val desc = c.internal.enclosingOwner.name.decodedName.toString
    
    implicit val symLiftable = symbolLiftable(c)
    
    if (params.exists(_.nonEmpty)) q"""new $typ(org.obl.bdd.Text($desc+" "+List(...$params).mkString(", ")), $f)"""
    else q"""new $typ(org.obl.bdd.Text($desc), $f)"""
  }
  
  def step[A : c.WeakTypeTag](c:Context)(f:c.Expr[A => A]):c.Tree = {
    import c.universe._
    
    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Step[$ta]", f.tree)
  }
  
  def source[A : c.WeakTypeTag](c:Context)(f:c.Expr[() => A]):c.Tree = {
    import c.universe._
    
    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Source[$ta]", f.tree)
  }
  
  def expectation[A : c.WeakTypeTag, E : c.WeakTypeTag](c:Context)(f:c.Expr[A => TestResult[E]]):c.Tree = {
    import c.universe._
    
    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Expectation[$ta,$te]", q"{ (state:$ta) => Seq($f(state)) }")
  }
  
}