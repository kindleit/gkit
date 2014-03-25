package gkit.sql

import shapeless._
import shapeless.ops.hlist._

case class Table[A <: HList](name: String, rowDef: A) {

  def map[B <: HList, C <: HList](f: A => B)(implicit row: Row.Aux[B, C]) =
    Query(this, f, None, Nil, None, None)

  def flatMap[B <: HList, C <: HList, D <: HList](f: A => Query[B, C, D])(implicit row: Row.Aux[C, D]) = {
    val q = f(*)
    q.copy(relations = name :: q.relations)
  }

  def filter[B <: HList](f: A => Exp[_])(implicit row: Row.Aux[A, B]) =
    Query(this, (_: A) => *, Some(f), Nil, None, None)

  def insert[B <: HList](b: B)(implicit row: Row.Aux[A, B], ts: ToStatement[B]) =
    Query(this, (_: A) => *, None, Nil, None, None).insert(b)

  def * : A = rowDef
}
