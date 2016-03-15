package me.yakovlev.lambda.types

import me.yakovlev.lambda.term.Term

/**
  * Created by pavel on 14.03.16.
  */
trait Type {
  def →(that : Type) : Type =
    Arrow(this, that)

  def free : Set[TVariable]
}

case class TVariable(s : String) extends Type {
  override def free : Set[TVariable] =
    Set(this)
}

case class Arrow(from : Type, to : Type) extends Type {
  override def free : Set[TVariable] =
    from.free union to.free
}

object Type {
  def apply(term : Term) : Type =
    Algorithm.pt(term)
}