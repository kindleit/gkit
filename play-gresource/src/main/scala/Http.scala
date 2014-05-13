package play.modules.gresource

import gkit._

import play.api.libs.json._

import play.api.http._

import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._

import scala.concurrent.{ExecutionContext, Future}

import scala.runtime.AbstractPartialFunction

import scalaz._
import scalaz.Scalaz._

case class RequestW[A, B, C](request: Request[A], params: B, body: C)

object Http {

  def mkRoute(method: String)(prefix: String) =
    Route(method, PathPattern(List(StaticPart(prefix))))

  def mkRouteWithId(method: String)(prefix: String) =
    Route(method, PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def json[A, B]
    (mkRoute: String => Route.ParamsExtractor)
    (run: RequestW[JsValue, A, B] => Future[String \/ SimpleResult])
    (implicit pc: ParamsCollector[A], jp: JSONPickler[B]) =
    new Op[JsValue, A, B](BodyParsers.parse.json)(mkRoute)(run)

  class Op[A, B, C]
    (bp: BodyParser[A])
    (mkRoute: String => Route.ParamsExtractor)
    (run: RequestW[A, B, C] => Future[String \/ SimpleResult])
    (implicit pc: ParamsCollector[B], p: Pickler[C, A])
      extends Routes with Results with BodyParsers with Status { self =>

    private var path: String = ""

    implicit def ec = play.api.libs.concurrent.Execution.Implicits.defaultContext

    def filter(f: Request[A] => Future[Boolean]): Op[A, B, C] =
      new Op[A, B, C](bp)(mkRoute)(run) {
        override def accept(r: Request[A]) = f(r)
        override def check(body: C) = self.check(body)
      }

    def validate(f: C => String \/ C) =
      new Op[A, B, C](bp)(mkRoute)(run) {
        override def accept(r: Request[A]) = self.accept(r)
        override def check(v: C) = f(v)
      }

    def action(rp: RouteParams): Handler = {

      def mkReqW(r: Request[A]) = for {
        ps <- pc.collect(rp)
        b1 <- p.unpickle(r.body)
        b2 <- check(b1)
      } yield RequestW(r, ps, b2)

      def mkReqWAndRun(r: Request[A]) =
        mkReqW(r).fold(
          e => Future(BadRequest(e)),
          rw => run(rw).map(_.fold(InternalServerError(_), identity)))

      def buildResult(r: Request[A]) =
        accept(r).flatMap(_.fold(mkReqWAndRun(r), Future(Forbidden)))

      Action.async(bp)(buildResult)
    }

    def accept(r: Request[A]): Future[Boolean] = Future(true)

    def check(v: C): String \/ C = v.right

    def routes = params(mkRoute(prefix)) >>> (action _).arrow[PartialFunction]

    def params(pe: Route.ParamsExtractor): PartialFunction[RequestHeader, RouteParams] = {
      case pe(rp) => rp
    }

    def orElse[D, E, F](op: Op[D, E, F]) = new Op[A, B, C](bp)(mkRoute)(run) {
      override def routes = new AbstractPartialFunction[RequestHeader, Handler] {
        op.setPrefix(prefix)
        self.setPrefix(prefix)
        override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) =
          op.routes.applyOrElse(rh, self.routes)
        override def isDefinedAt(rh: RequestHeader) =
          op.routes.isDefinedAt(rh) || self.routes.isDefinedAt(rh)
      }
    }

    def |:[D, E, F](op: Op[D, E, F]) = orElse(op)

    def prefix = path

    def setPrefix(prefix: String) = { path = prefix }

    def documentation = Seq()
  }
}
