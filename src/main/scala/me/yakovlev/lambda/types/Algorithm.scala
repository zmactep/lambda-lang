package me.yakovlev.lambda.types

import me.yakovlev.lambda.term.{Lambda, Application, Variable, Term}
import me.yakovlev.lambda.types.util.TypeWriter

/**
  * Created by pavel on 14.03.16.
  */
object Algorithm {
  def u(type1 : Type, type2 : Type) : Substitution =
    u(Equations(Set(type1 -> type2)))

  def u(equations : Equations) : Substitution =
    if (equations.isEmpty) {
      Substitution.empty
    }
    else {
      val (tt, equations_) = equations.split
      tt match {
        case (α@TVariable(_), τ) if α == τ =>
          u(equations_)
        case (α@TVariable(_), τ) if τ.free.contains(α) =>
          throw new IllegalArgumentException(s"Cannot be typed (${TypeWriter(α)} ~ ${TypeWriter(τ)})")
        case (α@TVariable(_), τ) =>
          val s = Substitution(Map(α -> τ))
          u(s(equations_)) o s
        case (τ, α@TVariable(_)) =>
          u(equations_ + (α, τ))
        case (Arrow(σ1, σ2), Arrow(τ1, τ2)) =>
          u(equations_ + (σ1, τ1) + (σ2, τ2))
      }
    }

  def e(c : Context, term : Term, σ : Type, fresh : Iterator[TVariable]) : Equations =
    term match {
      case x@Variable(_) =>
        Equations(Set(σ -> c(x)))
      case Application(m, n) =>
        val α = fresh.next()
        e(c, m, α → σ, fresh) union e(c, n, α, fresh)
      case Lambda(x, m) =>
        val α = fresh.next()
        val β = fresh.next()
        val cext = c.extend(x, α)
        e(cext, m, β, fresh) + (σ -> (α → β))
    }

  def pp(term : Term) : (Context, Type) = {
    val context = Context.fromTerm(term)
    val sigma = TVariable("r")

    val equations = e(context, term, sigma, Context.variables)
    val substitution = u(equations)

    (substitution(context), substitution(sigma))
  }

  def pt(term : Term) : Type =
    pp(term)._2
}
