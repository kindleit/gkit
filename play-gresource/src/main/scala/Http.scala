package play.modules.gresource

import play.api.libs.json._

import play.api.mvc._
import play.api.mvc.Results._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.runtime.AbstractPartialFunction

import scalaz._
import Scalaz._

object Http {

  sealed trait Error
  case class E400(msg: String) extends Error
  case class E404(msg: String) extends Error
  case class E403(msg: String) extends Error
  case class E500(msg: String) extends Error

  case class Req[A](underlying: Request[A], routeParams: RouteParams)

  case class Op[A]
    (bp: BodyParser[A])
    (mkRoute: String => Route.ParamsExtractor)
    (k: Kleisli[EFE, Req[A], SimpleResult])
      extends Routes { self =>
    private var path: String = ""
    lazy val route = mkRoute(prefix)

    def routes = {
      case route(rp) => Action.async(bp)(r => k.run(Req(r, rp)).leftMap(errorToResult).run.map(_.merge))
    }
    def prefix = path
    def setPrefix(prefix: String) = { path = prefix }
    def documentation = Seq()

    def orElse[B](op: Op[B]) = new Op[A](bp)(mkRoute)(k) {
      override def routes = new AbstractPartialFunction[RequestHeader, Handler] {
        op.setPrefix(prefix)
        self.setPrefix(prefix)
        override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) =
          op.routes.applyOrElse(rh, self.routes)
        override def isDefinedAt(rh: RequestHeader) =
          op.routes.isDefinedAt(rh) || self.routes.isDefinedAt(rh)
      }
    }

    def |:[B](op: Op[B]) = orElse(op)
  }

  sealed trait Path {
    def /(name: String) = new /(this, name)
    def /?(name: String) = new /?(this, name)
  }
  case object Prefix extends Path
  case class /(parent: Path, name: String) extends Path
  case class /?(parent: Path, name: String) extends Path

  class PathParam[A] {
    def apply[B](name: String)(implicit PB: PathBindable[A]) =
      liftK((r: Req[B]) => r.routeParams.fromPath[A](name).value.fold(e => (E400(e):Error).left, _.right).point[Future])
  }

  class ParamsFromReq[A] {
    def apply[B](implicit PC: ParamsCollector[A]): Kleisli[EFE, Req[B], A] =
      liftK((r: Req[B]) => PC.collect(r.routeParams).leftMap(E400(_):Error).point[Future])
  }

  type JReq = Req[JsValue]
  type AReq = Req[AnyContent]
  type EFE[A] = EitherT[Future, Error, A]

  def liftK[A, B](f: A => Future[Error \/ B]): Kleisli[EFE, A, B] =
    for {
      r <- Kleisli.ask[EFE, A]
      a <- EitherT(f(r)).liftM[({type λ[α[_],β]=Kleisli[α, A, β]})#λ]
    } yield a

  def idK[A]: Kleisli[EFE, A, A] =
    liftK((a: A) => Future(a.right))

  def constK[A, B](b: Future[Error \/ B]): Kleisli[EFE, A, B] =
    liftK((_: A) => b)

  def askAReq = Kleisli.ask[EFE, AReq]

  def askJReq = Kleisli.ask[EFE, JReq]

  def error[A]: Kleisli[EFE, Error, A] =
    liftK((e: Error) => Future(e.left))

  def accept[A](f: Req[A] => Future[Error \/ Boolean]): Kleisli[EFE, Req[A], Error \/ Req[A]] =
    liftK(r => f(r).map(_.map(_.fold(r.right, E403("").left))))

  def paramsFromReq[A]: ParamsFromReq[A] = new ParamsFromReq[A]

  def jsonFromReq[A](implicit JP: JSONPickler[A]): Kleisli[EFE, JReq, A] =
    liftK(r => JP.unpickle(r.underlying.body).leftMap(E400(_):Error).point[Future])

  def toJsonResult[A](implicit JP: JSONPickler[A]): Kleisli[EFE, A, SimpleResult] =
    Kleisli((a: A) => Ok(JP.pickle(a)).point[EFE])

  def optionToJsonResult[A](implicit JP: JSONPickler[A]): Kleisli[EFE, Option[A], SimpleResult] =
    Kleisli((oa: Option[A]) => oa.cata(a => Ok(JP.pickle(a)), NotFound).point[EFE])

  def errorToResult(e: Error): SimpleResult = e match {
    case E400(msg) => BadRequest(msg)
    case E403(msg) => Forbidden(msg)
    case E404(msg) => NotFound(msg)
    case E500(msg) => InternalServerError(msg)
  }

  def pathParam[A]: PathParam[A] = new PathParam[A]

  def mkPath(path: Path, prefix: String): List[PathPart] = path match {
    case Prefix         => StaticPart(prefix) :: Nil
    case /(path, name)  => mkPath(path, prefix) ::: List(StaticPart("/"), StaticPart(name))
    case /?(path, name) => mkPath(path, prefix) ::: List(StaticPart("/"), DynamicPart(name, ".+", false))
  }

  def mkRoute(method: String)(path: Path)(prefix: String) =
    Route(method, PathPattern(mkPath(path, prefix)))

  def get(path: Path)(k: Kleisli[EFE, AReq, SimpleResult]) =
    Op(BodyParsers.parse.anyContent)(mkRoute("GET")(path))(k)

  def delete(path: Path)(k: Kleisli[EFE, AReq, SimpleResult]) =
    Op(BodyParsers.parse.anyContent)(mkRoute("DELETE")(path))(k)

  def jsonPost(path: Path)(k: Kleisli[EFE, JReq, SimpleResult]) =
    Op(BodyParsers.parse.json)(mkRoute("POST")(path))(k)

  def jsonPut(path: Path)(k: Kleisli[EFE, JReq, SimpleResult]) =
    Op(BodyParsers.parse.json)(mkRoute("PUT")(path))(k)
}
