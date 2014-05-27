package gkit.mongo

import play.api.libs.iteratee.Enumerator

import reactivemongo.bson.BSONDocument

import reactivemongo.api._
import reactivemongo.core.commands._

import scala.concurrent.ExecutionContext

import scalaz.syntax.std.option._
import scalaz.syntax.either._

import shapeless._

final class Collection(db: DefaultDB, cname: String) {

  import BSON._

  class Distinct[A] {
    def apply[B](key: String, query: B = EmptyQ)
      (implicit bp1: BSONPickler[A], bp2: BSONPickler[B], ec: ExecutionContext) = {
      val r = runCommad(BSONDocument("distinct" -> cname, "key" -> key, "query" -> toBSONDoc(query)))
      r.map(_.get("values").cata(fromBSON[A](_), "field `values' not found".left))
    }
  }

  class Aggregate[A] {
    def apply[B <: HList](pipeline: B)
      (implicit pbp: BSONPickler[B], rbp: BSONPickler[A], ec: ExecutionContext) = {
      val r = runCommad(BSONDocument("aggregate" -> cname, "pipeline" -> toBSONArray(pipeline)))
      r.map(_.get("result").cata(fromBSON[A](_), "field `result' not found".left))
    }
  }

  class Tail[A] {
    def apply[B](q: B, awaitData: Boolean = true)
      (implicit bp1: BSONPickler[A], bp2: BSONPickler[B], ec: ExecutionContext): Enumerator[A] = {
      val opts = if (awaitData) QueryOpts().tailable.awaitData else QueryOpts().tailable
      QueryBuilder(db, db(cname).find(toBSONDoc(q)).options(opts)).cursor[A].enumerate()
    }
  }

  def insert[A](a: A)(implicit bp: BSONPickler[A], ec: ExecutionContext) =
    db.collection(cname).insert(toBSONDoc(a))

  def insertMany[A](as: List[A])(implicit bp: BSONPickler[A], ec: ExecutionContext) =
    db.collection(cname).bulkInsert(Enumerator(as.map(a => toBSONDoc(a)):_*))

  def update[Q, M](query: Q = EmptyQ, modifier: M = EmptyQ, upsert: Boolean = false, multi: Boolean = false)
    (implicit qbp: BSONPickler[Q], mbp: BSONPickler[M], ec: ExecutionContext) =
    db(cname).update(toBSONDoc(query), toBSONDoc(modifier), GetLastError(), upsert, multi)

  def find[Q, F](query: Q = EmptyQ, fields: F = EmptyQ)
    (implicit qbp: BSONPickler[Q], fbp: BSONPickler[F], ec: ExecutionContext): QueryBuilder =
    QueryBuilder(db, db(cname).find(toBSONDoc(query), toBSONDoc(fields)))

  def remove[Q](query: Q = EmptyQ, justOne: Boolean = false)
    (implicit qbp: BSONPickler[Q], ec: ExecutionContext) =
    db(cname).remove(toBSONDoc(query), GetLastError(), justOne)

  def count[Q, F](query: Q = EmptyQ, fields: F = EmptyQ)
    (implicit qbp: BSONPickler[Q], fbp: BSONPickler[F], ec: ExecutionContext) = {
    def toOpt(d: BSONDocument) = if (d.isEmpty) None else Some(d)
    db.command(Count(cname, toOpt(toBSONDoc(query)), toOpt(toBSONDoc(fields))))
  }

  def distinct[A] = new Distinct[A]

  def aggregate[A] = new Aggregate[A]

  def tail[A] = new Tail[A]

  def runCommad[C](cmd: C)(implicit bp: BSONPickler[C], ec: ExecutionContext) = {
    db.command(RawCommand(toBSONDoc(cmd)))
  }
}
