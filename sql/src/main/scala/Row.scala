package gkit.sql

import shapeless._
import shapeless.record._

trait Row[L <: HList] {
  type Repr <: HList
  def columns(l: L): List[Column[_]]
}

trait LowPriorityRow {
  type Aux[L <: HList, Repr0 <: HList] = Row[L] { type Repr = Repr0 }

  implicit def hnilRow[L <: HList]: Aux[L, HNil] =
    new Row[L] {
      type Repr = HNil
      def columns(l: L): List[Column[_]] = Nil
    }
}

object Row extends LowPriorityRow {
  def apply[L <: HList](implicit row: Row[L]) = row

  implicit def hlistRow[K, V, T <: HList]
    (implicit rt: Row[T]): Aux[FieldType[K, Column[V]] :: T, FieldType[K, V] :: rt.Repr] =
    new Row[FieldType[K, Column[V]] :: T] {
      type Repr = FieldType[K, V] :: rt.Repr
      def columns(l: FieldType[K, Column[V]] :: T): List[Column[_]] =
        (l.head: Column[V]) :: rt.columns(l.tail)
    }

  def like[L <: HList](l: L)(implicit row: Row[L]) = row
}
