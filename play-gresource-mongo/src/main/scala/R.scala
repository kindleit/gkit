package play.modules.gresource.mongo

import gkit.mongo._

import play.api.Play.current

import play.api.libs.json._

import play.api.mvc._
import play.api.mvc.Results._

import play.modules.gjson._
import play.modules.gresource._

import reactivemongo.core.commands.LastError

import scala.concurrent.{Future, ExecutionContext}

import scala.concurrent.ExecutionContext.Implicits.global

import scalaz._
import scalaz.Scalaz._

case class ResourceEnv[A, B, C](requestW: RequestW[A, B, C], dbe: DbEnv)

object R {
  object json extends JsonResourceFunctions
}

private[mongo] trait JsonResourceFunctions {

  def jsBody[A](implicit JP: JSONPickler[A]) =
    (r: Request[JsValue]) => JP.unpickle(r.body)

  def error[M[_]](implicit M: Monad[M]) =
    (e: String) => M.point(BadRequest(e))

  def fatalErrorOr[M[_], A](f: A => SimpleResult)(implicit M: Monad[M]) =
    (fa: M[String \/ A]) => EitherT.eitherT(fa).fold(InternalServerError(_), f)

  def insertFromReq[M[_], A, B](implicit M: Monad[M], K: Kleisli[M, A, String \/ B], JP: JSONPickler[A]) =
    jsBody[A] >>> (error[M] ||| (K.run(_)) >>> fatalErrorOr(_ => Ok))
}
