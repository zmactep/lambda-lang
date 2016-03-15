package me.yakovlev.lambda.types

/**
  * Created by pavel on 14.03.16.
  */
case class Substitution(substitution : Map[TVariable, Type]) {
  def apply(context : Context) : Context =
    Context(context.context.map { case (v, t) => v -> apply(t) })

  def apply(equations : Equations) : Equations =
    Equations(equations.eq.map { case (t1, t2) => apply(t1) -> apply(t2) })

  def apply(t : Type) : Type =
    t match {
      case Arrow(from, to) =>
        Arrow(apply(from), apply(to))
      case x@TVariable(_) =>
        substitution.getOrElse(x, x)
    }

  def keys : Set[TVariable] =
    substitution.keySet

  def o(that : Substitution) : Substitution = {
    val subs =
      (keys union that.keys)
        .map {
          case v =>
            v -> this(that(v))
        }
        .toMap

    Substitution(subs)
  }
}

object Substitution {
  val empty : Substitution = Substitution(Map.empty)
}
