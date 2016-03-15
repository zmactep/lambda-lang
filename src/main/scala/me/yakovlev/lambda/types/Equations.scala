package me.yakovlev.lambda.types

/**
  * User: pavel
  * Date: 15.03.16
  * Time: 12:50
  */
case class Equations(eq : Set[(Type, Type)]) {
  def isEmpty : Boolean =
    eq.isEmpty

  def split : ((Type, Type), Equations) = {
    val tt = eq.head
    (tt, this - tt)
  }

  def union(that : Equations) : Equations =
    Equations(eq union that.eq)

  def +(pair : (Type, Type)) : Equations =
    Equations(eq + pair)

  def -(pair : (Type, Type)) : Equations =
    Equations(eq - pair)

  def reduce : (Type, Type) = {
    val (first, second) = eq.toList.unzip
    (reduceTypeList(first), reduceTypeList(second))
  }

  private def reduceTypeList(types : List[Type]) : Type = {
    val rtypes = types.reverse
    rtypes.tail.foldLeft(rtypes.head) {
      case (t2, t1) => t1 → t2
    }
  }
}
