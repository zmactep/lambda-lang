package me.yakovlev.lambda.interactive

import me.yakovlev.lambda.term.{Term, Variable}

/**
  * User: pavel
  * Date: 14.03.16
  * Time: 9:48
  */
case class Environment(environment : List[(Variable, Term)], lastResult : Result) {
  def run(statement : Statement) : Environment = {
    statement match {
      case InvalidStatement(msg) =>
        Environment(environment, ErrorResult(msg))

      case ShowEnvironmentStatement =>
        Environment(environment, EnvironmentResult(this))
      case ClearEnvironmentStatement =>
        Environment.create

      case EnvFreeTermStatement(term) =>
        Environment(environment, TermResult(term))
      case ReduceEnvFreeTermStatement(term) =>
        Environment(environment, TermResult(term.reduce))

      case TermStatement(term) =>
        validate(term) match {
          case None => Environment(environment, TermResult(substitute(term)))
          case Some(error) => Environment(environment, ErrorResult(error))
        }
      case ReduceTermStatement(term) =>
        validate(term) match {
          case None => Environment(environment, TermResult(substitute(term).reduce))
          case Some(error) => Environment(environment, ErrorResult(error))
        }

      case BindStatement(v, term) =>
        validate(v) match {
          case None =>
            validate(term) match {
              case None => Environment((v, substitute(term)) :: environment, BindResult(v, substitute(term)))
              case Some(error) => Environment(environment, ErrorResult(error))
            }
          case Some(error) => Environment(environment, ErrorResult(error))
        }
    }
  }

  protected def substitute(term : Term) : Term =
    environment.foldRight(term) {
      case ((v, expr), nterm) =>
        nterm.substitute(v, expr)
    }

  protected def validate(variable : Variable) : Option[String] =
    !environment.exists(_._1 == variable) match {
      case true => None
      case false => Some(s"Variable '${variable.s}' is already bound.")
    }

  protected def validate(term : Term) : Option[String] =
    term.free.filterNot(v => environment.exists(_._1 == v)) match {
      case s if s.isEmpty => None
      case s => Some(s"Variable ${s.map(_.s).map(s => s"'$s'").head} is not bounded.")
    }
}

object Environment {
  def create : Environment = Environment(Nil, NoResult)
}