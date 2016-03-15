package me.yakovlev.lambda.test

import me.yakovlev.lambda.parsers.TermParser
import me.yakovlev.lambda.term.Variable
import me.yakovlev.lambda.types.{Context, Equations, Algorithm, TVariable}
import org.scalatest.{FlatSpec, Matchers}

/**
  * User: pavel
  * Date: 15.03.16
  * Time: 9:14
  */
class TypeAlgorithmTest extends FlatSpec with Matchers {
  private val α = TVariable("α")
  private val β = TVariable("β")
  private val γ = TVariable("γ")
  private val δ = TVariable("δ")
  private val ε = TVariable("ε")
  private val a0 = TVariable("a0")

  "U" should "find general substitution" in {
    val first = β -> ((γ → δ) → ε)
    val second = β -> (α → δ)

    val (t1, t2) = Equations(Set(first, second)).reduce
    val sub = Algorithm.u(t1, t2)

    sub(α) should be (γ → δ)
    sub(β) should be ((γ → δ) → δ)
    sub(ε) should be (δ)
  }

  "E" should "create equations for term" in {
    val term = TermParser("\\x.x y")
    val context = Context.fromTerm(term)
    val sigma = TVariable("r")

    context should be (Context(Map(Variable("y") -> a0)))

    val equations = Algorithm.e(context, term, sigma, Context.variables)
    val (t1, t2) = equations.reduce

    val sub = Algorithm.u(t1, t2)

    sub(sigma) should be ((a0 → β) → β)
  }
}
