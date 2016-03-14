package me.yakovlev.lambda.interactive

import me.yakovlev.lambda.term.{Term, Variable}

/**
  * User: pavel
  * Date: 14.03.16
  * Time: 9:01
  */
sealed trait Statement

case object ShowEnvironmentStatement extends Statement
case object ClearEnvironmentStatement extends Statement

case class InvalidStatement(error : String) extends Statement
case class BindStatement(variable : Variable, term : Term) extends Statement
case class TermStatement(term : Term) extends Statement
case class EnvFreeTermStatement(term : Term) extends Statement
case class ReduceTermStatement(term : Term) extends Statement
case class ReduceEnvFreeTermStatement(term : Term) extends Statement