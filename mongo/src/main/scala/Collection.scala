package gkit.mongo

import play.api.libs.iteratee.Enumerator

import reactivemongo.bson.BSONDocument

import reactivemongo.api.QueryOpts
import reactivemongo.core.commands.GetLastError
import reactivemongo.core.commands.Count
import reactivemongo.core.commands.RawCommand

import scalaz.syntax.std.option._
import scalaz.syntax.either._

import shapeless._

final class Collection(cname: String) {

  import BSON._

  def insert[A](a: A)(implicit dbe: DbEnv, bp: BSONPickler[A]) = {
    implicit val ec = dbe.executionContext
    dbe.db.collection(cname).insert(toBSONDoc(a))
  }

  def update[Q, M](query: Q = EmptyQ, modifier: M = EmptyQ, upsert: Boolean = false, multi: Boolean = false)
    (implicit dbe: DbEnv, qbp: BSONPickler[Q], mbp: BSONPickler[M]) = {
    implicit val ec = dbe.executionContext
    dbe.db(cname).update(toBSONDoc(query), toBSONDoc(modifier), GetLastError(), upsert, multi)
  }

  def find[Q, F](query: Q = EmptyQ, fields: F = EmptyQ)
    (implicit dbe: DbEnv, qbp: BSONPickler[Q], fbp: BSONPickler[F]): QueryBuilder = {
    QueryBuilder(dbe, dbe.db(cname).find(toBSONDoc(query), toBSONDoc(fields)))
  }

  def remove[Q](query: Q = EmptyQ, justOne: Boolean = false)
    (implicit dbe: DbEnv, qbp: BSONPickler[Q]) = {
    implicit val ec = dbe.executionContext
    dbe.db(cname).remove(toBSONDoc(query), GetLastError(), justOne)
  }

  def count[Q, F](query: Q = EmptyQ, fields: F = EmptyQ)
    (implicit dbe: DbEnv, qbp: BSONPickler[Q], fbp: BSONPickler[F]) = {
    implicit val ec = dbe.executionContext
    def toOpt(d: BSONDocument) = if (d.isEmpty) None else Some(d)
    dbe.db.command(Count(cname, toOpt(toBSONDoc(query)), toOpt(toBSONDoc(fields))))
  }

  def distinct[R] = new {
    def apply[Q](key: String, query: Q = EmptyQ)
      (implicit dbe: DbEnv, qbp: BSONPickler[Q], rbp: BSONPickler[R]) = {
      implicit val ec = dbe.executionContext
      val r = runCommad(BSONDocument("distinct" -> cname, "key" -> key, "query" -> toBSONDoc(query)))
      r.map(_.get("values").cata(fromBSON[R](_), "field `values' not found".left))
    }
  }

  def aggregate[R] = new {
    def apply[P <: HList](pipeline: P)
      (implicit dbe: DbEnv, pbp: BSONPickler[P], rbp: BSONPickler[R]) = {
      implicit val ec = dbe.executionContext
      val r = runCommad(BSONDocument("aggregate" -> cname, "pipeline" -> toBSONArray(pipeline)))
      r.map(_.get("result").cata(fromBSON[R](_), "field `result' not found".left))
    }
  }

  def runCommad[C](cmd: C)(implicit dbe: DbEnv, bp: BSONPickler[C]) = {
    implicit val ec = dbe.executionContext
    dbe.db.command(RawCommand(toBSONDoc(cmd)))
  }

  def tail[A] = new {
    def apply[B](q: B, awaitData: Boolean = true)
      (implicit dbe: DbEnv, abp: BSONPickler[A], bbp: BSONPickler[B]): Enumerator[A] = {
        implicit val ec = dbe.executionContext
        val opts = if (awaitData) QueryOpts().tailable.awaitData else QueryOpts().tailable
        QueryBuilder(dbe, dbe.db(cname).find(toBSONDoc(q)).options(opts)).cursor[A].enumerate()
      }
  }
}

object collection {
  def apply(cname: String) = new Collection(cname)
}
