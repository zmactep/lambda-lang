package me.yakovlev.lambda.parsers

import me.yakovlev.lambda.interactive._

/**
  * User: pavel
  * Date: 14.03.16
  * Time: 8:56
  */
trait InteractiveParser extends TermParser {
  def statement : Parser[Statement] =
    ( bindS
    | typeTermS
    | reduceTermS
    | reduceEfTermS
    | subsTermS
    | showS
    | efTermS
    | clearS
    | termS
    )

  def termS : Parser[TermStatement] =
    term ^^ {
      case term =>
        TermStatement(term)
    }

  def typeTermS : Parser[TypeTermStatement] =
    ":t" ~> term ^^ {
      case term =>
        TypeTermStatement(term)
    }

  def reduceTermS : Parser[ReduceTermStatement] =
    ":r" ~> term ^^ {
      case term =>
        ReduceTermStatement(term)
    }

  def subsTermS : Parser[SubstituteTermStatement] =
    ":s" ~> term ^^ {
      case term =>
        SubstituteTermStatement(term)
    }

  def efTermS : Parser[EnvFreeTermStatement] =
    ":e" ~> term ^^ {
      case term =>
        EnvFreeTermStatement(term)
    }

  def reduceEfTermS : Parser[ReduceEnvFreeTermStatement] =
    ":er" ~> term ^^ {
      case term =>
        ReduceEnvFreeTermStatement(term)
    }

  def bindS : Parser[BindStatement] =
    "let" ~ variable ~ "=" ~ term ^^ {
      case _ ~ v ~ _ ~ term =>
        BindStatement(v, term)
    }

  def showS : Parser[Statement] =
    ":env" ^^ {
      case _ =>
        ShowEnvironmentStatement
    }

  def clearS : Parser[Statement] =
    ":clear" ^^ {
      case _ =>
        ClearEnvironmentStatement
    }
}

object InteractiveParser extends InteractiveParser {
  def apply(line : String) : Statement =
    parseAll(statement, line) match {
      case Success(st, _) => st
      case NoSuccess(msg, _) => InvalidStatement(msg)
    }
}