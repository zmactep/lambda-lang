package me.yakovlev.lambda.types.util

import me.yakovlev.lambda.types.{Arrow, TVariable, Type}

/**
  * Created by pavel on 14.03.16.
  */
object TypeWriter {
  def apply(t : Type) : String =
    write(t, isFrom = false)

  private[this] def write(t : Type, isFrom : Boolean) : String =
    t match {
      case TVariable(s) => s
      case Arrow(from, to) =>
        val s = s"${write(from, isFrom = true)} -> ${write(to, isFrom = false)}"
        if (isFrom) s"($s)" else s
    }
}
