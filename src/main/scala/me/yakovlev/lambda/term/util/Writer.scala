package me.yakovlev.lambda.term.util

import me.yakovlev.lambda.term.{Lambda, Application, Variable, Term}

/**
  * Created by pavel on 14.03.16.
  */
object Writer {
  def apply(term : Term) : String =
    write(term, Unknown, Unknown)

  private[this] def write(term : Term, iam : Iam, parentis : Iam) : String =
    term match {
      case Variable(x) =>
        iam match {
          case LambdaBody =>
            s".$x"
          case _ =>
            x
        }
      case Application(e1, e2) =>
        val s = s"${write(e1, ApplicationAlgo, iam)} ${write(e2, ApplicationData, iam)}"
        iam match {
          case LambdaBody =>
            s".$s"
          case ApplicationData =>
            s"($s)"
          case _ =>
            s
        }
      case Lambda(Variable(x), e) =>
        val s = s"$x${write(e, LambdaBody, iam)}"
        iam match {
          case Unknown =>
            s"λ$s"
          case LambdaBody =>
            s" $s"
          case ApplicationAlgo =>
            s"(λ$s)"
          case ApplicationData =>
            parentis match {
              case ApplicationAlgo =>
                s"(λ$s)"
              case _ =>
                s"λ$s"
            }
        }
    }

  private[this] sealed trait Iam
  private[this] case object Unknown extends Iam
  private[this] case object LambdaBody extends Iam
  private[this] case object ApplicationAlgo extends Iam
  private[this] case object ApplicationData extends Iam
}
