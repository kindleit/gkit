package play.modules.gresource.sql

import gkit.sql._

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson.JSON.toJSON
import play.modules.gjson._
import play.modules.gresource._

import scalaz.effect._
import scalaz.std.effect.AllEffectInstances._
import scalaz.syntax.std.option._

import shapeless._
import shapeless.ops.record._

object Find {
  def apply[A <: HList, B <: HList](table: Table[A])
    (implicit
      db    : DB
    , r     : Row.Aux[A, B]
    , frs   : FromResultSet[B]
    , jp    : JSONPickler[B]
    )
    = new Op {

      lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

      def executionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

      def action(rp: RouteParams) = {
        val r = for {
          skip <- rp.fromQuery[Int]("skip", Some(0)).value.right
          limit <- rp.fromQuery[Int]("limit", Some(10)).value.right
        } yield find(skip, limit)
        r.left.map(badRequest).merge
      }

      def find(skip: Int, limit: Int) = Action { _ =>
        val q = table.map(a => a).drop(skip).take(limit).toList
        val k = q.map(_.fold(InternalServerError(_), xs => Ok(toJSON(xs))))
        IO(db.getConnection).using(k).unsafePerformIO()
      }
    }

  def apply[A <: HList, B, C <: HList](table: Table[A], pidf: Witness.Aux[B])
    (implicit
      db    : DB
    , pidfs : Selector.Aux[A, B, Column[Int]]
    , r     : Row.Aux[A, C]
    , frs   : FromResultSet[C]
    , jp    : JSONPickler[C]
    )
    = new Op
    {
      lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

      def executionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

      def action(rp: RouteParams) = {
        val r = for {
          pid <- rp.fromQuery[Int]("pid", None).value.right
          skip <- rp.fromQuery[Int]("skip", Some(0)).value.right
          limit <- rp.fromQuery[Int]("limit", Some(10)).value.right
        } yield find(pid, skip, limit)
        r.left.map(badRequest).merge
      }

      def find(pid: Int, skip: Int, limit: Int) = Action { _ =>
        val q = table.filter(_.get(pidf) === pid).drop(skip).take(limit).toList
        val k = q.map(_.fold(InternalServerError(_), xs => Ok(toJSON(xs))))
        IO(db.getConnection).using(k).unsafePerformIO()
      }
    }
}

case class FindOne[A <: HList, B, C <: HList](table: Table[A], idf: Witness.Aux[B])
  (implicit
    db    : DB
  , idfs  : Selector.Aux[A, B, Column[Int]]
  , r     : Row.Aux[A, C]
  , frs   : FromResultSet[C]
  , jp    : JSONPickler[C]
  ) extends Op {

  lazy val route =
    Route("GET", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", "[0-9]+", false))))

  def executionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def action(rp: RouteParams) =
    rp.fromPath[Int]("id", None).value.fold(badRequest, findOne)

  def findOne(id: Int) = Action { _ =>
    val q = table.filter(_.get(idf) === id).first
    val k = q.map(_.fold(InternalServerError(_), _.cata(x => Ok(toJSON(x)), NotFound)))
    IO(db.getConnection).using(k).unsafePerformIO()
  }
}
