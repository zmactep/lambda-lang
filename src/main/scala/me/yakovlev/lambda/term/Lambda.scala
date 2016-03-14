package me.yakovlev.lambda.term

/**
  * Created by pavel on 13.03.16.
  */
case class Lambda(variable : Variable, expr : Term) extends Term {
  override def free : Set[Variable] =
    expr.free - variable

  override def bounded : Set[Variable] =
    expr.bounded + variable

  override def alpha(conflicts : Set[Variable]) : Term = {
    if (conflicts.contains(variable)) {
      val nv = variable.rename(conflicts union bounded)
      Lambda(nv, expr.substitute(variable, nv))
    }
    else {
      Lambda(variable, expr.alpha(conflicts))
    }
  }

  override def beta : Term =
    Lambda(variable, expr.beta)

  override def eta : Term =
    expr match {
      case Application(f, y) if variable == y && !f.free.contains(variable) => f
      case _ => this
    }

  override def substitute(v : Variable, nexpr : Term) : Term =
    if (v != variable) {
      if (nexpr.free.contains(variable)) {
        alpha(nexpr.free).substitute(v, nexpr)
      }
      else {
        Lambda(variable, expr.substitute(v, nexpr))
      }
    }
    else {
      this
    }

  override protected[term] def redexCount : Int =
    expr.redexCount
}
