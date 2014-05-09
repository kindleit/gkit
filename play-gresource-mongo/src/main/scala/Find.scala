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

class Find[A, B](cname: String, mkQuery: (Request[AnyContent], A, Collection) => Future[String \/ B])
  (implicit ec: ExecutionContext , pc: ParamsCollector[A] , jsp: JSONPickler[B])
    extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    val dbe = GMongoPlugin.dbEnv

    def buildResult(r: Request[AnyContent]) =
      pc.collect(rp).fold(e => Future(BadRequest(e)), p => {
        val c = dbe.collection(cname)
        mkQuery(r, p, c).map(_.fold(InternalServerError(_), x => Ok(toJSON(x))))
      })

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Find[A, B](cname, mkQuery) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Find {

  def apply[A, B](cname: String, mkQuery: (Request[AnyContent], B, Collection) => Future[String \/ A])
    (implicit ec: ExecutionContext, pc: ParamsCollector[B], bp: JSONPickler[A]) =
    new Find[B, A](cname, mkQuery)

  case class DefaultParams(skip: Option[Int], limit: Option[Int])

  def mkDefaultQry[A](implicit ec: ExecutionContext, bp: BSONPickler[A], jp: JSONPickler[A]) = {
    (r: Request[AnyContent], dp: DefaultParams, c: Collection) => {
      val q = c.find(EmptyQ).drop(dp.skip | 0).take(dp.limit | 10)
      for {
        d  <- q.cursor[A].collect[List]()
        st <- c.count(EmptyQ)
        t  <- c.count(EmptyQ)
      } yield d.map { xs =>
        "data" ->> xs ::
        "meta" ->> ("subtotal" ->> st :: "total" ->> t :: HNil) ::
        HNil
      }
    }
  }
}
