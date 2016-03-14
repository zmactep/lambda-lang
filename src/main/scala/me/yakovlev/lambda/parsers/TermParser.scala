package me.yakovlev.lambda.parsers

import me.yakovlev.lambda.term.{Application, Lambda, Term, Variable}

import scala.util.parsing.combinator._

/**
  * Created by pavel on 13.03.16.
  */
trait TermParser extends RegexParsers {
  def term : Parser[Term] =
    ( application
    | nonapplication
    )

  def nonapplication : Parser[Term] =
    ( variable
    | lambda
    | "(" ~> term <~ ")"
    )

  def application : Parser[Application] =
    nonapplication ~ rep1(nonapplication) ^^ {
      case t1 ~ terms =>
        terms.foldLeft(t1) {
          case (app, t) =>
            Application(app, t)
        }.asInstanceOf[Application]
    }

  def lambda : Parser[Lambda] =
    "(λ|\\\\)".r ~ rep1(variable) ~ "." ~ term ^^ {
      case _ ~ vars ~ _ ~ t =>
        vars.foldRight(t) {
          case (v, expr) =>
            Lambda(v, expr)
        }.asInstanceOf[Lambda]
    }

  def variable : Parser[Variable] =
    "[a-zA-Z]+(\\*|')?".r ^^ {
      case v => Variable(v)
    }
}

object TermParser extends TermParser {
  def apply(line : String) : Term =
    parseAll(term, line) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(s"Cannot parse: $msg")
    }
}