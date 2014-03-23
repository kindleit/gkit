package play.modules.gresource.sql

import gkit.sql._

import play.modules.gjson._
import play.modules.gresource._

import shapeless._
import shapeless.ops.record._

object Resource {
  def apply[A <: HList, B, C, D <: HList, E <: HList, F <: HList]
    (table: Table[A], idf: Witness.Aux[B])
    (implicit
      db   : DB
    , idfs : Selector.Aux[A, idf.T, Column[Int]]
    , idrm : Remover.Aux[A, B, (C, D)]
    , r1   : Row.Aux[A, E]
    , r2   : Row.Aux[D, F]
    , ts   : ToStatement[F]
    , frs1 : FromResultSet[E]
    , frs2 : FromResultSet[F]
    , jp1  : JSONPickler[E]
    , jp2  : JSONPickler[F]
    )
    = Filter(table) |: Insert(table, idf) |: Filter1(table, idf) |: Update(table, idf) |: Delete(table, idf)

  def apply[A <: HList, B, C, D, E <: HList, F <: HList, G <: HList]
    (table: Table[A], idf: Witness.Aux[B], pidf: Witness.Aux[C])
    (implicit
      db   : DB
    , idfs : Selector.Aux[A, B, Column[Int]]
    , pidfs: Selector.Aux[A, C, Column[Int]]
    , idrm : Remover.Aux[A, B, (D, E)]
    , r1   : Row.Aux[A, F]
    , r2   : Row.Aux[E, G]
    , ts   : ToStatement[G]
    , frs1 : FromResultSet[F]
    , frs2 : FromResultSet[G]
    , jp1  : JSONPickler[F]
    , jp2  : JSONPickler[G]
    )
    = Filter(table, pidf) |: Insert(table, idf) |: Filter1(table, idf) |: Update(table, idf) |: Delete(table, idf)
}
