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

import shapeless._
import shapeless.ops.record._

case class Delete[A <: HList, B, C <: HList](table: Table[A], idf: Witness.Aux[B])
  (implicit
    db   : DB
  , idfs : Selector.Aux[A, B, Column[Int]]
  , r    : Row.Aux[A, C]
  ) extends Op {

  lazy val route =
    Route("DELETE", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", "[0-9]+", false))))

  def mkResponse(params: RouteParams) =
    call(params.fromPath[Int]("id", None))(delete)

  def delete(id: Int) = Action { _ =>
    val q = table.filter(_.get(idf) === id).delete
    val k = q.map(x => Ok(toJSON(x)))
    IO(db.getConnection).using(k).unsafePerformIO()
  }
}
