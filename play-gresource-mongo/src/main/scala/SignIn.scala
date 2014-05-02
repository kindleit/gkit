package play.modules.gresource.mongo

import gkit.mongo._

import play.api.libs.json._

import play.api.Play.current

import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson.JSON._
import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

case class SignIn[A, B]
  (
    cname     : String
  , getQuery  : Collection => A => QueryBuilder
  , getResult : Request[JsValue] => B => Future[SimpleResult]
  )
  (implicit
    ec  : ExecutionContext
  , bsp1 : BSONPickler[A]
  , bsp2 : BSONPickler[B]
  , jsp1 : JSONPickler[A]
  ) extends Op[JsValue] {

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    val dbe = GMongoPlugin.dbEnv

    def getValue(r: Request[JsValue]) =
      r.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

    def signIn(r: Request[JsValue])(a: A) =
      getQuery(dbe.collection(cname))(a).one[B].flatMap(_.cata(getResult(r), Future(Unauthorized)))

    buildAction(parse.json)(r => getValue(r).fold(e => Future(BadRequest(e)), signIn(r)))
  }
}
