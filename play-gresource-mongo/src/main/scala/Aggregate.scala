package play.modules.gresource.mongo

import gkit.mongo._

import play.modules.gjson._
import play.modules.gresource._

import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.Future

import scalaz._
import Scalaz._

import shapeless._

class Aggregate[A, B, C <: HList]
  (
    cname: String
  , mkPipeline: Request[AnyContent] => B => Future[String \/ C]
  )
  (implicit
    dbe: DbEnv
  , bp1: BSONPickler[A]
  , bp3: BSONPickler[C]
  , jsp: JSONPickler[A]
  , pc: ParamsCollector[B]
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  implicit val executionContext = dbe.executionContext

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def action(rp: RouteParams) = {

    def toStatus(r: String \/ A) =
      r.fold(InternalServerError(_), as => Ok(toJSON(as)))

    def aggregate(r: Request[AnyContent])(ps: B) =
      mkPipeline(r)(ps).flatMap(_.fold(e => Future(e.left), x => collection(cname).aggregate[A](x)))

    def buildResult(r: Request[AnyContent]) =
      pc.collect(rp).fold(e => Future(BadRequest(e)), ps => aggregate(r)(ps).map(toStatus))

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Aggregate[A, B, C](cname, mkPipeline) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Aggregate {
  def apply[A] = new {
    def apply[B, C <: HList]
      (
        cname: String
      , mkPipeline: Request[AnyContent] => B => Future[String \/ C]
      )
      (implicit
        dbe: DbEnv
      , rbp: BSONPickler[A]
      , bsp: BSONPickler[C]
      , jsp: JSONPickler[A]
      , pc: ParamsCollector[B]
      )
      = new Aggregate[A, B, C](cname, mkPipeline)
  }
}
