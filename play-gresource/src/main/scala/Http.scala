package play.modules.gresource.mongo

import play.api.libs.json._

import play.api.mvc._
import play.api.mvc.Results._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}

import scala.concurrent.ExecutionContext.Implicits.global

import scalaz._
import scalaz.Scalaz._

sealed trait Error
case class Fatal(msg: String) extends Error
case class Nonfatal(msg: String) extends Error

object Http {

  type ES[F[_], A] = EitherT[F, Error, A]
  type EIS[A] = EitherT[Id, Error, A]
  type KEISR[A] = Kleisli[EIS, Request[AnyContent], A]
  type KR[F[_], A] = Kleisli[F, Request[AnyContent], A]

  def jsValueFromReq[A](implicit JP: JSONPickler[A]): KEISR[A] =
    Kleisli.local[EIS, A, Request[AnyContent]](identity)(for {
      r <- Kleisli.ask[EIS, Request[JsValue]]
      a <- EitherT(JP.unpickle(r.body).leftMap(Nonfatal(_):Error).point[Id]).liftM[KR]
    } yield a)

  def paramsFromReq[A](rp: RouteParams)(implicit PC: ParamsCollector[A]): KEISR[A] = for {
    r <- Kleisli.ask[EIS, Request[AnyContent]]
    a <- EitherT(PC.collect(rp).leftMap(Nonfatal(_):Error).point[Id]).liftM[KR]
  } yield a

  def jsValueResult[M[_], A](implicit M: Monad[M], JP: JSONPickler[A]): Kleisli[M, A, SimpleResult] =
    Kleisli((a: A) => M.point(Ok(JP.pickle(a))))

  def doWith[A, B, C](rp: RouteParams)(k: Kleisli[EIS, (A, B), C])
    (implicit PC: ParamsCollector[A], JP: JSONPickler[B]): Kleisli[EIS, Request[AnyContent], C] =
    (paramsFromReq[A](rp) &&& jsValueFromReq[B]) >>> k

  def mkGetRoute(prefix: String) = Route("GET", PathPattern(List(StaticPart(prefix))))

  def get[A](k: Kleisli[EIS, A, Future[SimpleResult]])
    (implicit PC: ParamsCollector[A]) =
    OpWrapper(BodyParsers.parse.anyContent)(mkGetRoute) { (rp, req) =>
      (paramsFromReq(rp) >>> k).run(req).run.fold({
        case Fatal(msg) => Future(InternalServerError(msg))
        case Nonfatal(msg) => Future(BadRequest(msg))
      }, identity)
    }

  case class OpWrapper[A]
    (bp: BodyParser[A])
    (mkRoute: String => Route.ParamsExtractor)
    (run: (RouteParams, Request[A]) => Future[SimpleResult])
      extends Routes {
    private var path: String = ""
    lazy val route = mkRoute(prefix)
    def routes = { case route(rp) => Action.async(bp)(run(rp, _)) }
    def prefix = path
    def setPrefix(prefix: String) = { path = prefix }
    def documentation = Seq()
  }
}
