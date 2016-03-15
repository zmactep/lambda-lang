package me.yakovlev.lambda.interactive

import me.yakovlev.lambda.term.{Term, Variable}
import me.yakovlev.lambda.types.Algorithm

/**
  * User: pavel
  * Date: 14.03.16
  * Time: 9:48
  */
case class Environment(environment : Map[Variable, (Term, Int)], lastResult : Result) {
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

      case TypeTermStatement(term) =>
        try {
          val sterm = substitute(term)
          val (context, ttype) = Algorithm.pp(term)
          Environment(environment, TermContextTypeResult(sterm, context, ttype))
        }
        catch {
          case e : IllegalArgumentException =>
            Environment(environment, ErrorResult(e.getMessage))
        }

      case TermStatement(term) =>
        validate(term) match {
          case None => Environment(environment, TermResult(term))
          case Some(error) => Environment(environment, ErrorResult(error))
        }
      case SubstituteTermStatement(term) =>
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
              case None => Environment(environment + (v -> (term, environment.size)), BindResult(v, term))
              case Some(error) => Environment(environment, ErrorResult(error))
            }
          case Some(error) => Environment(environment, ErrorResult(error))
        }
    }
  }

  protected def substitute(term : Term) : Term =
    if (term.free.nonEmpty) {
      environment.toIndexedSeq.sortBy(_._2._2).foldRight(term) {
        case ((v, (expr, _)), nterm) =>
          nterm.substitute(v, expr)
      }
    }
    else term

  protected def validate(variable : Variable) : Option[String] =
    !environment.contains(variable) match {
      case true => None
      case false => Some(s"Variable '${variable.s}' is already bound.")
    }

  protected def validate(term : Term) : Option[String] =
    term.free.filterNot(environment.contains) match {
      case s if s.isEmpty => None
      case s => Some(s"Variable ${s.map(_.s).map(s => s"'$s'").head} is not bounded.")
    }
}

object Environment {
  def create : Environment = Environment(Map.empty, NoResult)
}