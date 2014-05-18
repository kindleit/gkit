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
import Scalaz._

object Http {

  sealed trait Error
  case class E400(msg: String) extends Error
  case class E403(msg: String) extends Error
  case class E500(msg: String) extends Error

  case class Req[A](underlying: Request[A], routeParams: RouteParams)

  type JReq = Req[JsValue]
  type AReq = Req[AnyContent]
  type EFE[A] = EitherT[Future, Error, A]

  def liftK[A, B](f: A => Future[Error \/ B]): Kleisli[EFE, A, B] =
    for {
      r <- Kleisli.ask[EFE, A]
      a <- EitherT(f(r)).liftM[({type λ[α[_],β]=Kleisli[α, A, β]})#λ]
    } yield a

  def error[A]: Kleisli[EFE, Error, A] =
    liftK((e: Error) => Future(e.left))

  def filter[A](f: Req[A] => Future[Error \/ Boolean]): Kleisli[EFE, Req[A], Error \/ Req[A]] =
    liftK(r => f(r).map(_.map(_.fold(r.right, E403("").left))))

  def paramsFromReq[A, B](implicit PC: ParamsCollector[B]): Kleisli[EFE, Req[A], B] =
    liftK((r: Req[A]) => PC.collect(r.routeParams).leftMap(E400(_):Error).point[Future])

  def jsonFromReq[A](implicit JP: JSONPickler[A]): Kleisli[EFE, JReq, A] =
    liftK(r => JP.unpickle(r.underlying.body).leftMap(E400(_):Error).point[Future])

  def jsonToResult[A](implicit JP: JSONPickler[A]): Kleisli[EFE, A, SimpleResult] =
    Kleisli((a: A) => Ok(JP.pickle(a)).point[EFE])

  def errorToResult(e: Error): SimpleResult = e match {
    case E400(msg) => BadRequest(msg)
    case E403(msg) => Forbidden(msg)
    case E500(msg) => InternalServerError(msg)
  }

  def pathSuffix[A](name: String)(implicit PB: PathBindable[A]) =
    liftK((r: AReq) => r.routeParams.fromPath[A](name).value.fold(e => (E400(e):Error).left, _.right).point[Future])

  def mkRoute(method: String)(prefix: String) =
    Route(method, PathPattern(List(StaticPart(prefix))))

  def get(k: Kleisli[EFE, AReq, SimpleResult]) =
    Op(BodyParsers.parse.anyContent)(mkRoute("GET"))(k)

  def delete(k: Kleisli[EFE, AReq, SimpleResult]) =
    Op(BodyParsers.parse.anyContent)(mkRoute("DELETE"))(k)

  def jsonPost(k: Kleisli[EFE, JReq, SimpleResult]) =
    jsonOp(mkRoute("POST"))(k)

  def jsonPut(k: Kleisli[EFE, JReq, SimpleResult]) =
    jsonOp(mkRoute("PUT"))(k)

  def jsonOp(mkRoute: String => Route.ParamsExtractor)(k: Kleisli[EFE, JReq, SimpleResult]) =
    Op(BodyParsers.parse.json)(mkRoute)(k)

  def find[A, B](f: A => Future[Error \/ B])
    (implicit PC: ParamsCollector[A], JP: JSONPickler[B]): Kleisli[EFE, AReq, SimpleResult] =
    paramsFromReq[AnyContent, A] >>> liftK(f) >>> jsonToResult[B]

  def insert[A, B](f: A => Future[Error \/ B])
    (implicit JP1: JSONPickler[A], JP2: JSONPickler[B]): Kleisli[EFE, JReq, SimpleResult] =
    jsonFromReq[A] >>> liftK(f) >>> jsonToResult[B]

  case class Op[A]
    (bp: BodyParser[A])
    (mkRoute: String => Route.ParamsExtractor)
    (k: Kleisli[EFE, Req[A], SimpleResult])
      extends Routes {
    private var path: String = ""
    lazy val route = mkRoute(prefix)
    def routes = {
      case route(rp) => Action.async(bp)(r => k.run(Req(r, rp)).leftMap(errorToResult).run.map(_.merge))
    }
    def prefix = path
    def setPrefix(prefix: String) = { path = prefix }
    def documentation = Seq()
  }
}
