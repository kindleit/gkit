package play.modules.gresource.mongo

import gkit.Generator

import gkit.mongo._

import play.api.libs.json._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.LastError

import scala.concurrent.Future

import scalaz._
import Scalaz._

case class Insert[A, ID](cname: String)
  (implicit
    dbe  : DbEnv
  , bsp  : BSONPickler[A]
  , jsp1 : JSONPickler[A]
  , jsp2 : JSONPickler[ID]
  , gen  : Generator[ID]
  ) extends Op[JsValue] {

  import play.modules.gjson.JSON._

  implicit val executionContext = dbe.executionContext

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def action(rp: RouteParams) = {

    def getValue(r: Request[JsValue]): String \/ A =
      r.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

    def insert(id: ID)(a: A) = collection(cname).insert(a)

    def getStatus(id: ID)(f: Future[LastError]) =
      f.map(le => le.ok.fold(Ok(toJSON(id)), InternalServerError(~le.errMsg)))

    def buildResult(r: Request[JsValue]) = {
      val id = gen.generate
      getValue(r).fold(e => Future(BadRequest(e)), insert(id) _ >>> getStatus(id) _)
    }

    buildAction(parse.json)(buildResult)
  }

  def filter(f: Request[JsValue] => Future[Boolean]) =
    new Insert[A, ID](cname) {
      override def accept(r: Request[JsValue]) = f(r)
    }
}
