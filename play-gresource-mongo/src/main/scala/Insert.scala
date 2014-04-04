package play.modules.gresource.mongo

import gkit.Generator

import gkit.mongo._

import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.LastError

import scalaz._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

import scala.concurrent.Future

case class Insert[A, ID](cname: String)
  (implicit
    dbe  : DbEnv
  , bsp  : BSONPickler[A]
  , jsp1 : JSONPickler[A]
  , jsp2 : JSONPickler[ID]
  , gen  : Generator[ID]
  ) extends Op {

  import play.modules.gjson.JSON._

  implicit val ec = dbe.executionContext

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = insert

  def filter(f: RequestHeader => Boolean) = new Insert[A, ID](cname) {
    override def _filter = { case rh => f(rh) }
  }

  def insert = Action.async(BodyParsers.parse.json) { req =>
    val id = gen.generate
    fromRequest(id, req).fold(e => Future(BadRequest(e)), a => doInsert(a).map(mkAction(id)))
  }

  def fromRequest(id: ID, req: Request[JsValue]): String \/ A =
    req.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

  def doInsert(a: A) = collection(cname).insert(a)

  def mkAction(id: ID)(le: LastError) =
    le.ok.fold(Ok(toJSON(id)), InternalServerError(~le.errMsg))
}
