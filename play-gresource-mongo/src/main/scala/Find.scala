package play.modules.gresource.mongo

import gkit.mongo._

import org.joda.time.DateTime

import play.api.Play.current

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}

import scalaz._
import scalaz.syntax.std.option._

import shapeless._
import shapeless.syntax.singleton._

class Find[A, B, C, D]
  (
    cname: String
  , mkQuery: (Request[AnyContent], Collection, B) => Future[(QueryBuilder, C, D)]
  )
  (implicit
    ec: ExecutionContext
  , bsp1: BSONPickler[A]
  , bsp2: BSONPickler[C]
  , bsp3: BSONPickler[D]
  , jsp: JSONPickler[A]
  , pc: ParamsCollector[B]
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    val dbe = GMongoPlugin.dbEnv

    def find(r: Request[AnyContent], p: B) = for {
      (q, a, b) <- mkQuery(r, dbe.collection(cname), p)
      d         <- q.cursor[A].collect[List]()
      st        <- dbe.collection(cname).count(a)
      t         <- dbe.collection(cname).count(b)
    } yield d.map { xs =>
      "data" ->> xs ::
      "meta" ->> ("subtotal" ->> st :: "total" ->> t :: HNil) ::
      HNil
    }

    def buildResult(r: Request[AnyContent]) =
      pc.collect(rp).fold(
        e => Future(BadRequest(e)),
        p => find(r, p).map(_.fold(InternalServerError(_), x => Ok(toJSON(x)))))

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Find[A, B, C, D](cname, mkQuery) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Find {
  def apply[A] = new {
    def apply[B, C, D]
      (
        cname: String
      , mkQuery: (Request[AnyContent], Collection, B) => Future[(QueryBuilder, C, D)]
      )
      (implicit
        ec: ExecutionContext
      , bsp1: BSONPickler[A]
      , bsp2: BSONPickler[C]
      , bsp3: BSONPickler[D]
      , jsp: JSONPickler[A]
      , bspj: BSONProj[A]
      , pc: ParamsCollector[B]
      )
      = new Find[A, B, C, D](cname, mkQuery)
  }

  case class DefaultParams(skip: Option[Int], limit: Option[Int])

  def mkDefaultQry(implicit ex: ExecutionContext) =
    (r: Request[AnyContent], c: Collection, dp: DefaultParams) => {
      Future((c.find(EmptyQ).drop(dp.skip | 0).take(dp.limit | 10), EmptyQ, EmptyQ))
    }
}
