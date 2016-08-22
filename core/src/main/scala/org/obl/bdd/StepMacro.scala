package org.obl.bdd

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@macrocompat.bundle
class StepMacro(val c: blackbox.Context) {

  import c.universe._

  private def filePosition: Tree = {
    val line = c.enclosingPosition.line
    val path = c.enclosingPosition.source.file.path
    q"new org.obl.bdd.FilePosition($path, $line)"
  }

  private def ownerDescription: Tree = {
    val enclosingOwner = c.internal.enclosingOwner
    val params = if (enclosingOwner.isMethod) {
      val firstParams = enclosingOwner.asMethod.paramLists.headOption.getOrElse(Nil)
      if (firstParams.exists(_.isImplicit)) Nil else firstParams
    } else Nil

    val desc = enclosingOwner.name.decodedName.toString

    if (params.nonEmpty) q"""$desc+" "+List(..${params.map(_.asTerm.name)}).mkString(", ")"""
    else q"$desc"
  }

  private def newDescriptionAndParameterType(typ: Tree, f: Tree): Tree = {
    val desc = ownerDescription
    val fpos = filePosition
    c.typecheck( q"""new $typ(org.obl.bdd.Text(None, $desc), $f, Some($fpos))""" )
  }

  private def newSelfDescribeLike(typ: Tree, f: Tree): Tree = {
    val desc = ownerDescription
    val fpos = filePosition
    c.typecheck( q"""new $typ($desc, $f, Some($fpos))""" )
  }

  private def newSelfDescribeLikeDescriptionProvided(typ: Tree, desc: Tree, f: Tree): Tree = {
    val fpos = filePosition
    c.typecheck( q"""new $typ($desc, $f, Some($fpos))""" )
  }

  def step[A: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    newDescriptionAndParameterType(tq"org.obl.bdd.Step[$ta]", f)
  }

  def source[A: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(tq"org.obl.bdd.Source[$ta]", f)
  }

  def expectation[A: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    newDescriptionAndParameterType(tq"org.obl.bdd.Expectation[$ta,$te]", q"{ (state:$ta) => Seq($f(state)) }")
  }

  def selfDescribe[A: c.WeakTypeTag, B: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val tb = c.weakTypeOf[B]

    newSelfDescribeLike(tq"org.obl.bdd.SelfDescribeF1[$ta, $tb]", f)
  }

  def scenario[S: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[S]
    val tb = c.weakTypeOf[E]

    newSelfDescribeLike(tq"org.obl.bdd.Scenario[$ta, $tb]", f)
  }

  def scenarioDescriptionProvided[S: c.WeakTypeTag, E: c.WeakTypeTag](description: Tree, f: Tree): Tree = {
    val ta = c.weakTypeOf[S]
    val tb = c.weakTypeOf[E]

    newSelfDescribeLikeDescriptionProvided(tq"org.obl.bdd.Scenario[$ta, $tb]", description, f)
  }

  def predicate[A: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]

    newDescriptionAndParameterType(tq"org.obl.bdd.Predicate[$ta]", f)
  }

}