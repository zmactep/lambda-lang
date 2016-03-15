package me.yakovlev.lambda.types

import me.yakovlev.lambda.term.{Term, Variable}

/**
  * Created by pavel on 14.03.16.
  */
case class Context(context : Map[Variable, Type]) {
  private lazy val types : Set[TVariable] =
    context.values.foldLeft(Set.empty[TVariable]) {
      case (set, t) =>
        set union t.free
    }

  def apply(variable : Variable) : Type =
    context(variable)

  def extend(v : Variable, t : Type) : Context =
    Context(context + (v -> t))

  def fresh(additional : Set[TVariable] = Set.empty) : TVariable = {
    val busy = types ++ additional
    Context.variables.find(v => !busy.contains(v)).get
  }
}

object Context {
  private val greek = "αβγδεζηθικλμνξοπρστυφχψω".toList

  val empty : Context = Context(Map.empty)

  def variables : Iterator[TVariable] = {
    Iterator.from(0).flatMap {
      case idx =>
        Iterator(greek:_*)
          .map(_.toString)
          .map {
            case name =>
              if (idx > 0) name + idx else name
          }
          .map(TVariable.apply)
    }
  }

  def fromTerm(term : Term) : Context =
    Context(term.free.zipWithIndex.map { case (v, idx) => v -> TVariable(s"a$idx") }.toMap)
}