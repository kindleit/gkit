package play.modules.gresource.mongo

import gkit.mongo._

import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.LastError

import scala.concurrent.Future

import scalaz._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

case class Update[A, ID](cname: String)
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , idp: BSONPickler[ID]
  , pb:  PathBindable[ID]
  ) extends Op {

  import play.modules.gjson.JSON._

  implicit val ec = dbe.executionContext

  lazy val route =
    Route("PUT", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def mkResponse(params: RouteParams) = call(params.fromPath[ID]("id"))(update)

  def filter(f: RequestHeader => Boolean) = new Update[A, ID](cname) {
    override def _filter = { case rh => f(rh) }
  }

  def update(id: ID) = Action.async(BodyParsers.parse.json) { req =>
    fromRequest(req).fold(e => Future(BadRequest(e)), a => doUpdate(id, a).map(mkAction))
  }

  def fromRequest(req: Request[JsValue]): String \/ A =
    req.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

  def doUpdate(id: ID, a: A) = {
    val b = BSONDocument(BSON.toBSONDoc(a).elements.filter(_._1 != "_id"))
    collection(cname).update(IdQ(id), Set(b))
  }

  def mkAction(le: LastError) =
    le.ok.fold(Ok, InternalServerError(~le.errMsg))
}
