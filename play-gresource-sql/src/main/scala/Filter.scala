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

object Filter {
  def apply[A <: HList, B <: HList](table: Table[A])
    (implicit
      db    : DB
    , r     : Row.Aux[A, B]
    , frs   : FromResultSet[B]
    , jp    : JSONPickler[B]
    )
    = new Op {

      lazy val paramsExt = Route("GET", PathPattern(List(StaticPart(prefix))))

      def routes = {
        case paramsExt(params) =>
          call(params.fromQuery[Int]("skip", Some(0)), params.fromQuery[Int]("limit", Some(10)))(filter)
      }

      def filter(skip: Int, limit: Int) = Action { _ =>
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
      lazy val paramsExt = Route("GET", PathPattern(List(StaticPart(prefix))))

      def routes = {
        case paramsExt(params) =>
          call(
            params.fromQuery[Int]("pid", None),
            params.fromQuery[Int]("skip", Some(0)),
            params.fromQuery[Int]("limit", Some(10)))(filter)
      }

      def filter(pid: Int, skip: Int, limit: Int) = Action { _ =>
        val q = table.filter(_.get(pidf) === pid).drop(skip).take(limit).toList
        val k = q.map(_.fold(InternalServerError(_), xs => Ok(toJSON(xs))))
        IO(db.getConnection).using(k).unsafePerformIO()
      }
    }
}

case class Filter1[A <: HList, B, C <: HList](table: Table[A], idf: Witness.Aux[B])
  (implicit
    db    : DB
  , idfs  : Selector.Aux[A, B, Column[Int]]
  , r     : Row.Aux[A, C]
  , frs   : FromResultSet[C]
  , jp    : JSONPickler[C]
  ) extends Op {

  lazy val paramsExt =
    Route("GET", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", "[0-9]+", false))))

  def routes = {
    case paramsExt(params) => call(params.fromPath[Int]("id", None))(filter1)
  }

  def filter1(id: Int) = Action { _ =>
    val q = table.filter(_.get(idf) === id).first
    val k = q.map(_.fold(InternalServerError(_), _.cata(x => Ok(toJSON(x)), NotFound)))
    IO(db.getConnection).using(k).unsafePerformIO()
  }
}
