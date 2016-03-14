package me.yakovlev.lambda.interactive

import me.yakovlev.lambda.term.util.Writer
import me.yakovlev.lambda.term.{Term, Variable}

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
    s"${v.s} = ${Writer(term)}"
}

case class EnvironmentResult(env : Environment) extends Result {
  override def show : String =
    env.environment
      .map {
        case (v, term) =>
          s"${v.s} = ${Writer(term)}"
      }
      .mkString("\n")
}

case class TermResult(term : Term) extends Result {
  override def show : String =
    Writer(term)
}