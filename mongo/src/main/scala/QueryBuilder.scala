package gkit.mongo

import reactivemongo.api._
import reactivemongo.api.collections.GenericQueryBuilder

import reactivemongo.bson.{ BSONValue, BSONObjectID, BSONDocument, BSONDocumentReader, BSONDocumentWriter }

import scalaz._
import scalaz.std.string._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._

case class QueryBuilder(
  dbe: DbEnv,
  queryBuilder: GenericQueryBuilder[BSONDocument, BSONDocumentReader, BSONDocumentWriter],
  upTo: Int = Int.MaxValue) {

  import BSON._

  implicit val ec = dbe.executionContext

  implicit def reader[A](implicit bp: BSONPickler[A]) = new BSONDocumentReader[A] {
    def read(doc: BSONDocument) =
      fromBSON[A](doc).valueOr(e => throw new RuntimeException(e))
  }

  def sort[A](order: A)(implicit bp: BSONPickler[A]): QueryBuilder =
    copy(queryBuilder = queryBuilder.sort(toBSONDoc(order)))

  def take(n: Int): QueryBuilder =
    copy(queryBuilder = queryBuilder.options(QueryOpts(batchSizeN = n)), upTo = n)

  def drop(n: Int): QueryBuilder =
    copy(queryBuilder = queryBuilder.options(QueryOpts(skipN = n)))

  def skip(n: Int): QueryBuilder = drop(n)

  def limit(n: Int): QueryBuilder = take(n)

  def cursor[A](implicit bp: BSONPickler[A]) = Cursor(dbe, queryBuilder.cursor[A], upTo)

  def one[A](implicit bp: BSONPickler[A]) = queryBuilder.one[A]
}
