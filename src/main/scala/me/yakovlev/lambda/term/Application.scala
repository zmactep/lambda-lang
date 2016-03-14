package me.yakovlev.lambda.term

/**
  * Created by pavel on 13.03.16.
  */
case class Application(algo : Term, data : Term) extends Term {
  override def free : Set[Variable] =
    algo.free union data.free

  override def bounded : Set[Variable] =
    algo.bounded union data.bounded

  override def alpha(conflicts : Set[Variable]) : Term =
    Application(algo.alpha(conflicts), data.alpha(conflicts))

  override def beta : Term = {
    algo match {
      case Lambda(v, expr) =>
        expr.substitute(v, data)
      case _ =>
        val algoBeta = algo.beta
        if (algo != algoBeta) Application(algoBeta, data) else Application(algo, data.beta)
    }
  }

  override def substitute(v : Variable, expr : Term) : Term =
    Application(algo.substitute(v, expr), data.substitute(v, expr))

  override protected[term] def redexCount : Int =
    algo match {
      case Lambda(_, expr) =>
        1 + expr.redexCount + data.redexCount
      case _ =>
        algo.redexCount + data.redexCount
    }
}
