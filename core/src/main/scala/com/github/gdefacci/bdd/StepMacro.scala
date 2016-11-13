package com.github.gdefacci.bdd

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@macrocompat.bundle
class StepMacro(val c: blackbox.Context) {

  import c.universe._

  private def filePosition: Tree = {
    val line = c.enclosingPosition.line
    val path = c.enclosingPosition.source.file.path
    q"new com.github.gdefacci.bdd.FilePosition($path, $line)"
  }

  private def ownerDescription: Tree = {
    val enclosingOwner = c.internal.enclosingOwner
    val params = if (enclosingOwner.isMethod) {
      val firstParams = enclosingOwner.asMethod.paramLists.headOption.getOrElse(Nil)
      if (firstParams.exists(_.isImplicit)) Nil else firstParams
    } else Nil

    val desc = enclosingOwner.name.decodedName.toString

    val parsTxtsTree = if (params.length < 2) {
      params.map { p =>
        Ident(p.asTerm.name)
      }
    } else {
      params.map { p =>
        val prfx = Literal(Constant(p.asTerm.name.toString))
        val id = Ident(p.asTerm.name)
        q"""$prfx + " = " + $id"""
      }
    }

    if (params.nonEmpty) q"""$desc+" "+List(..$parsTxtsTree).mkString(" and ")"""
    else q"$desc"
  }

  def description = {
    val desc = ownerDescription
    q"com.github.gdefacci.bdd.Text(None, $desc)"
  }

  def position = q"Some($filePosition)"

  def step[A: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]

    q"com.github.gdefacci.bdd.Step[$ta]($description, $f, $position)"
  }

  def source[A: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    q"new com.github.gdefacci.bdd.Flow[$ta, $te]($description, $f, Nil, $position)"
  }

  def expectation[A: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    q"new com.github.gdefacci.bdd.Expectation[$ta,$te]($description, { state => Seq($f(state)) }, $position)"
  }

  def expectations[A: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val te = c.weakTypeOf[E]

    q"new com.github.gdefacci.bdd.Expectation[$ta,$te]($description, { state => $f(state) }, $position)"
  }

  def selfDescribe[A: c.WeakTypeTag, B: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]
    val tb = c.weakTypeOf[B]

    q"new com.github.gdefacci.bdd.SelfDescribeF1[$ta, $tb]($ownerDescription, $f, $position)"
  }

  def scenario[S: c.WeakTypeTag, E: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[S]
    val te = c.weakTypeOf[E]

    q"new com.github.gdefacci.bdd.Scenario[$ta, $te]($ownerDescription, $f, $position)"
  }

  def scenarioDescriptionProvided[S: c.WeakTypeTag, E: c.WeakTypeTag](description: Tree, f: Tree): Tree = {
    val ta = c.weakTypeOf[S]
    val tb = c.weakTypeOf[E]

    q"new com.github.gdefacci.bdd.Scenario[$ta, $tb]($description, $f, $position)"
  }

  def predicate[A: c.WeakTypeTag](f: Tree): Tree = {
    val ta = c.weakTypeOf[A]

    q"new com.github.gdefacci.bdd.Predicate[$ta]($description, $f, $position)"
  }

  def withPosition(f: Tree): Tree = {
    q"$f($filePosition)"
  }

  def withDescription(f: Tree): Tree = {
    q"$f($ownerDescription)"
  }

  def withDescriptionAndPosition(f: Tree): Tree = {
    q"$f($ownerDescription, $filePosition)"
  }

}