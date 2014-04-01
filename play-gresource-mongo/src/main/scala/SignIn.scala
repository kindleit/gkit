package play.modules.gresource.mongo

import gkit.mongo._

import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import scala.concurrent.Future

case class SignIn[A, B]
  (
    cname     : String
  , mkQuery   : (A, Collection) => QueryBuilder
  , mkSession : (B, Request[JsValue]) => Session
  )
  (implicit
    dbe  : DbEnv
  , bsp1 : BSONPickler[A]
  , bsp2 : BSONPickler[B]
  , jsp1 : JSONPickler[A]
  , jsp2 : JSONPickler[B]
  ) extends Op {

  import play.modules.gjson.JSON._

  implicit val ec = dbe.executionContext

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = signIn

  def signIn = Action.async(BodyParsers.parse.json) { req =>
    fromRequest(req).fold(e => Future(BadRequest(e)), a => trySignIn(a).map(mkAction(req)))
  }

  def fromRequest(req: Request[JsValue]): String \/ A =
    req.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

  def trySignIn(a: A) = mkQuery(a, collection(cname)).one[B]

  def mkAction(req: Request[JsValue])(a: Option[B]) =
    a.cata(x => Ok(toJSON(x)).withSession(mkSession(x, req)), Unauthorized)
}
