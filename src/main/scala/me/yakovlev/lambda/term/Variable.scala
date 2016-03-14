package me.yakovlev.lambda.term

/**
  * Created by pavel on 13.03.16.
  */
case class Variable(s : String) extends Term {
  override def free : Set[Variable] =
    Set(this)

  override def bounded : Set[Variable] =
    Set.empty

  override def alpha(conflicts : Set[Variable]) : Term = this

  def rename(conflicts : Set[Variable]) : Variable = {
    if (!conflicts.contains(this)) {
      this
    }
    else {
      ('a' until 'z')
        .map(_.toString)
        .map(Variable.apply)
        .find(!conflicts.contains(_))
        .getOrElse(Variable(s + "'").rename(conflicts))
    }
  }

  override def substitute(v : Variable, expr : Term) : Term =
    if (v == this) expr else this

  override protected[term] def redexCount : Int = 0
}