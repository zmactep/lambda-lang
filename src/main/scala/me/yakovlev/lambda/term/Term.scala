package me.yakovlev.lambda.term

/**
  * Created by pavel on 13.03.16.
  */
trait Term {
  def free : Set[Variable]
  def bounded : Set[Variable]

  def alpha(conflicts : Set[Variable]) : Term
  def beta : Term = this
  def eta : Term = this

  def reduce : Term =
    if (beta != this) {
      beta.reduce
    }
    else {
      if (eta != this) {
        eta.reduce
      }
      eta
    }

  def substitute(v : Variable, expr : Term) : Term

  protected[term] def redexCount : Int
}




