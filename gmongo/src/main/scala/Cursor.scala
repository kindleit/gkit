package gmongo

import reactivemongo.core.errors.DatabaseException

import play.api.libs.iteratee.Enumerator

import scala.concurrent.Future
import scala.collection.generic.CanBuildFrom

import scalaz._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._
import scalaz.syntax.validation._

case class Cursor[T](dbe: DbEnv, cursor: reactivemongo.api.Cursor[T]) {

  implicit val ec = dbe.executionContext

  def enumerate(maxDocs: Int = Int.MaxValue, stopOnError: Boolean = false): Enumerator[T] =
    cursor.enumerate(maxDocs, stopOnError)

  def collect[M[_]](upTo: Int = Int.MaxValue, stopOnError: Boolean = true)
    (implicit cbf: CanBuildFrom[M[_], T, M[T]]): Future[String \/ M[T]] =
    cursor.collect[M](upTo, stopOnError).map(_.right).recover {
      case de: DatabaseException => (~de.code.map(_.toString)).left
      case e: Throwable          => e.getMessage.left
    }
}
