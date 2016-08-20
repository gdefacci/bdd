package org.obl.bdd

import scala.reflect.macros.blackbox.Context

object StepMacro {

  def filePosition(c:Context):c.Tree = {
    import c.universe._

    val line = c.enclosingPosition.line
    val path = c.enclosingPosition.source.file.path
    q"new org.obl.bdd.FilePosition($path, $line)"
  }
  
  def ownerDescription(c: Context): c.Tree = {
    import c.universe._

    val enclosingOwner = c.internal.enclosingOwner
    val params = if (enclosingOwner.isMethod) {
      val firstParams = enclosingOwner.asMethod.paramLists.headOption.getOrElse(Nil)
      if (firstParams.exists(_.isImplicit)) Nil else firstParams
    } else Nil

    val desc = enclosingOwner.name.decodedName.toString

    implicit val symbolLiftable = c.universe.Liftable[c.Symbol] { sym =>
      import c.universe._
      q"${sym.asTerm.name}"
    }

    if (params.nonEmpty) q"""$desc+" "+List(..$params).mkString(", ")"""
    else q"$desc"
  }

  private def newDescriptionAndParameterType(c: Context)(typ: c.Tree, f: c.Tree) = {
    import c.universe._

    val desc = ownerDescription(c)
    val fpos = filePosition(c)
    q"""new $typ(org.obl.bdd.Text(None, $desc), $f, Some($fpos))"""
  }

  private def newSelfDescribeLike(c: Context)(typ: c.Tree, f: c.Tree) = {
    import c.universe._

    val desc = ownerDescription(c)
    val fpos = filePosition(c)
    q"""new $typ($desc, $f, Some($fpos))"""
  }
  
  private def newSelfDescribeLikeDescriptionProvided(c: Context)(typ: c.Tree, desc:c.Tree, f: c.Tree) = {
    import c.universe._

    val fpos = filePosition(c)
    q"""new $typ($desc, $f, Some($fpos))"""
  }


  def step[A: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    
    val ta = c.weakTypeOf[A]
    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Step[$ta]", f)
  }

  def source[A: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Source[$ta]", f)
  }

  def expectation[A: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Expectation[$ta,$te]", q"{ (state:$ta) => Seq($f(state)) }")
  }

  def selfDescribe[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]
    val tb = c.weakTypeOf[B]

    newSelfDescribeLike(c)(tq"org.obl.bdd.SelfDescribeF1[$ta, $tb]", f)
  }
  
  def scenario[S: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[S]
    val tb = c.weakTypeOf[E]

    newSelfDescribeLike(c)(tq"org.obl.bdd.Scenario[$ta, $tb]", f)
  }
  
  def scenarioDescriptionProvided[S: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(description:c.Tree, f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[S]
    val tb = c.weakTypeOf[E]

    newSelfDescribeLikeDescriptionProvided(c)(tq"org.obl.bdd.Scenario[$ta, $tb]", description, f)
  }
  
  
  def predicate[A: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(c)(tq"org.obl.bdd.Predicate[$ta]", f)
  }

}