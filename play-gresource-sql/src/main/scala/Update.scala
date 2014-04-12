package play.modules.gresource.sql

import gkit.sql._

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson.JSON.{toJSON, fromJSON}
import play.modules.gjson._
import play.modules.gresource._

import scalaz.effect._
import scalaz.std.effect.AllEffectInstances._

import shapeless._
import shapeless.ops.record._

case class Update[A <: HList, B, C, D <: HList, E <: HList](table: Table[A], idf: Witness.Aux[B])
  (implicit
    db   : DB
  , idfs : Selector.Aux[A, B, Column[Int]]
  , idrm : Remover.Aux[A, B, (C, D)]
  , r    : Row.Aux[D, E]
  , ts   : ToStatement[E]
  , frs  : FromResultSet[E]
  , jp   : JSONPickler[E]
  ) extends Op {

  lazy val route =
    Route("PUT", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", "[0-9]+", false))))

  def executionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  def action(rp: RouteParams) = {
    def doUpdate(id: Int)(a: E) = {
      val q = table.map(_ - idf).filter(_.get(idf) === id).update(a)
      val k = q.map(_.fold(InternalServerError(_), ru => Ok(toJSON(ru))))
      IO(db.getConnection).using(k).unsafePerformIO()
    }
    def update(id: Int) = Action(BodyParsers.parse.json) { req =>
      fromJSON[E](req.body).bimap(BadRequest(_), doUpdate(id)).merge
    }
    rp.fromPath[Int]("id", None).value.fold(badRequest, update)
  }
}
