package play.modules.gresource.mongo

import gkit.Generator

import gkit.mongo._

import play.api.Play.current

import play.api.libs.json._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.bson.{BSONObjectID, BSONDocument}
import reactivemongo.core.commands.LastError

import scala.concurrent.{Future, ExecutionContext}

import scalaz._
import Scalaz._

case class Insert[A, ID](cname: String)
  (implicit
    ec   : ExecutionContext
  , bsp1 : BSONPickler[A]
  , bsp2 : BSONPickler[ID]
  , jsp1 : JSONPickler[A]
  , jsp2 : JSONPickler[ID]
  , gen  : Generator[ID]
  ) extends Op[JsValue] {

  import play.modules.gjson.JSON._

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    implicit val dbe = GMongoPlugin.dbEnv

    def getValue(r: Request[JsValue]): String \/ A =
      r.body.asOpt[JsObject].cata(fromJSON[A], "invalid json value".left[A])

    def insert(id: ID)(a: A) = {
      val b = BSONDocument(("_id", BSON.toBSON(id))) ++
      BSONDocument(BSON.toBSONDoc(a).elements.filter(_._1 != "_id"))
      collection(cname).insert(b)
    }

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
