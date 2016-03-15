package me.yakovlev.lambda.interactive

import me.yakovlev.lambda.term.util.TermWriter
import me.yakovlev.lambda.term.{Term, Variable}
import me.yakovlev.lambda.types.{Context, Type}
import me.yakovlev.lambda.types.util.TypeWriter

/**
  * User: pavel
  * Date: 14.03.16
  * Time: 9:53
  */
trait Result {
  def show : String
}

case object NoResult extends Result {
  override def show : String = ""
}

case class ErrorResult(msg : String) extends Result {
  override def show : String =
    s"Error: $msg"
}

case class BindResult(v : Variable, term : Term) extends Result {
  override def show : String =
    s"${v.s} = ${TermWriter(term)}"
}

case class EnvironmentResult(env : Environment) extends Result {
  override def show : String =
    env.environment
      .map {
        case (v, (term, _)) =>
          s"${v.s} = ${TermWriter(term)}"
      }
      .mkString("\n")
}

case class TermResult(term : Term) extends Result {
  override def show : String =
    TermWriter(term)
}

trait TypeResultTrait {
  protected def typePair(v : Term, t : Type) : String =
    s"${TermWriter(v)}: ${TypeWriter(t)}"
}

case class TermContextTypeResult(term : Term, context : Context, ttype : Type) extends Result with TypeResultTrait {
  override def show : String =
    s"${context.context.map { case (v, t) => typePair(v, t) }.mkString(", ")} ⊢ ${typePair(term, ttype)}"
}

case class TermTypeResult(term : Term, ttype : Type) extends Result with TypeResultTrait {
  override def show : String =
    typePair(term, ttype)
}