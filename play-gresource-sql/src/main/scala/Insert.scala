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

case class Insert[A <: HList, B, C, D <: HList, E <: HList](table: Table[A], idf: Witness.Aux[B])
  (implicit
    db   : DB
  , idrm : Remover.Aux[A, B, (C, D)]
  , r    : Row.Aux[D, E]
  , ts   : ToStatement[E]
  , frs  : FromResultSet[E]
  , jp   : JSONPickler[E]
  ) extends Op {

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = call(insert)

  def insert = Action(BodyParsers.parse.json) { req =>
    fromJSON[E](req.body).bimap(BadRequest(_), doInsert).merge
  }

  def doInsert(a: E) = {
    val q = table.map(_ - idf).insert(a)
    val k = q.map(_.fold(InternalServerError(_), ru => Ok(toJSON(ru))))
    IO(db.getConnection).using(k).unsafePerformIO()
  }
}
