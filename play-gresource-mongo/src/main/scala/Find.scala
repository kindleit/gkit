package play.modules.gresource.mongo

import gkit.mongo._

import org.joda.time.DateTime

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.Future

import scalaz._
import scalaz.syntax.std.option._

import shapeless._
import shapeless.syntax.singleton._

class Find[A, B, C]
  (
    cname: String
  , mkQuery: (Request[AnyContent], Collection, B) => Future[QueryBuilder]
  , mkCountQuery: (Request[AnyContent], B) => Future[C]
  )
  (implicit
    dbe: DbEnv
  , bsp1: BSONPickler[A]
  , bsp2: BSONPickler[C]
  , jsp: JSONPickler[A]
  , bspj: BSONProj[A]
  , pc: ParamsCollector[B]
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  implicit val executionContext = dbe.executionContext

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def action(rp: RouteParams) = {

    def find(r: Request[AnyContent], p: B) = for {
      d <- mkQuery(r, collection(cname), p).flatMap(_.cursor[A].collect[List]())
      c <- mkCountQuery(r, p).flatMap(collection(cname).count(_))
    } yield d.map(xs => ("data" ->> xs :: ("meta" ->> ("count" ->> c :: HNil)) :: HNil))

    def buildResult(r: Request[AnyContent]) =
      pc.collect(rp).fold(
        e => Future(BadRequest(e)),
        p => find(r, p).map(_.fold(InternalServerError(_), x => Ok(toJSON(x)))))

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Find[A, B, C](cname, mkQuery, mkCountQuery) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Find {
  def apply[A] = new {
    def apply[B, C]
      (
        cname: String
      , mkQuery: (Request[AnyContent], Collection, B) => Future[QueryBuilder]
      , mkCountQuery: (Request[AnyContent], B) => Future[C]
      )
      (implicit
        dbe: DbEnv
      , bsp1: BSONPickler[A]
      , bsp2: BSONPickler[C]
      , jsp: JSONPickler[A]
      , bspj: BSONProj[A]
      , pc: ParamsCollector[B]
      )
      = new Find[A, B, C](cname, mkQuery, mkCountQuery)
  }

  case class DefaultParams(skip: Option[Int], limit: Option[Int])

  def mkDefaultQry(implicit dbe: DbEnv) = {
    implicit val ec = dbe.executionContext
    (r: Request[AnyContent], c: Collection, dp: DefaultParams) => {
      Future(c.find(EmptyQ).drop(dp.skip | 0).take(dp.limit | 10))
    }
  }

  def mkDefaultCntQry(implicit dbe: DbEnv) = {
    implicit val ec = dbe.executionContext
    (_: Request[AnyContent], _:Find.DefaultParams) => Future(EmptyQ)
  }
}
