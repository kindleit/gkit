package play.modules.gresource.mongo

import gkit.mongo._

import play.api.Play.current
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.LastError

import scala.concurrent.{Future, ExecutionContext}

import scalaz._
import Scalaz._

case class Update[A, ID](cname: String)
  (implicit
    ec: ExecutionContext
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , idp: BSONPickler[ID]
  , pb:  PathBindable[ID]
  ) extends Op[JsValue] {

  import play.modules.gjson.JSON._

  lazy val route =
    Route("PUT", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    implicit val dbe = GMongoPlugin.dbEnv

    def getId = rp.fromPath[ID]("id").value

    def getValue(r: Request[JsValue]): String \/ A =
      r.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

    def update(id: ID)(a: A) = {
      val b = BSONDocument(BSON.toBSONDoc(a).elements.filter(_._1 != "_id"))
      collection(cname).update(IdQ(id), Set(b))
    }

    def getStatus(fle: Future[LastError]) =
      fle.map(le => le.ok.fold(Ok, InternalServerError(~le.errMsg)))

    def f(r: Request[JsValue])(id: ID) =
      getValue(r).fold(e => Future(BadRequest(e)), update(id) _ >>> getStatus _)

    def buildResult(r: Request[JsValue]) =
      getId.fold(e => Future(BadRequest(e)), f(r))

    buildAction(parse.json)(buildResult)
  }

  def filter(f: Request[JsValue] => Future[Boolean]) = new Update[A, ID](cname) {
    override def accept(r: Request[JsValue]) = f(r)
  }
}
