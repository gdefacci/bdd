package org.obl.bdd

import scala.reflect.macros.blackbox.Context

object StepMacro {

  private def symbolLiftable(c: Context) = c.universe.Liftable[c.Symbol] { sym =>
    import c.universe._
    q"${sym.asTerm.name}"
  }

  private def newDescriptionAndParameterType(c: Context)(typ: c.Tree, f: c.Tree) = {
    import c.universe._

    val params = c.internal.enclosingOwner.asMethod.paramLists

    val desc = c.internal.enclosingOwner.name.decodedName.toString

    implicit val symLiftable = symbolLiftable(c)

    if (params.exists(_.nonEmpty)) q"""new $typ(org.obl.bdd.Text(None, $desc+" "+List(...$params).mkString(", ")), $f)"""
    else q"""new $typ(org.obl.bdd.Text(None, $desc), $f)"""
  }
  
  private def newSelfDescribeLike(c: Context)(typ: c.Tree, f: c.Tree) = {
    import c.universe._

    val params = c.internal.enclosingOwner.asMethod.paramLists

    val desc = c.internal.enclosingOwner.name.decodedName.toString

    implicit val symLiftable = symbolLiftable(c)

    if (params.exists(_.nonEmpty)) q"""new $typ($desc+" "+List(...$params).mkString(", "), $f)"""
    else q"""new $typ($desc, $f)"""
  }

  def step[A: c.WeakTypeTag](c: Context)(f: c.Expr[A => A]): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Step[$ta]", f.tree)
  }

  def source[A: c.WeakTypeTag](c: Context)(f: c.Expr[() => A]): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Source[$ta]", f.tree)
  }

  def expectation[A: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(f: c.Expr[A => TestResult[E]]): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Expectation[$ta,$te]", q"{ (state:$ta) => Seq($f(state)) }")
  }
  
  def selfDescribe[A: c.WeakTypeTag,B: c.WeakTypeTag](c: Context)(f: c.Expr[A => B]): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]
    val tb = c.weakTypeOf[B]

    newSelfDescribeLike(c)(tq"org.obl.bdd.SelfDescribeF1[$ta, $tb]", f.tree)
  }


}