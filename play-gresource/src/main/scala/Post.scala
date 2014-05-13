package play.modules.gresource

import gkit._

import play.api.libs.json._

import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._

import scala.concurrent.{ExecutionContext, Future}

import scalaz.\/
import scalaz.syntax.either._

class Post[A, B, C](bp: BodyParser[A])(run: Request[A] => B => C => Future[String \/ SimpleResult])
  (implicit
    ec: ExecutionContext
  , p: Pickler[C, A]
  , pc: ParamsCollector[B]
  ) extends Op[A] { self =>

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    def badReq(e: String) = Future(BadRequest(e))

    def buildResult(r: Request[A]) = {
      (for {
        ps <- pc.collect(rp)
        v  <- p.unpickle(r.body)
      } yield (ps, v)).fold(badReq, {
        case (ps, v) => check(v).flatMap(_.fold(badReq,
          run(r)(ps)(_).map(_.fold(InternalServerError(_), identity))))
      })
    }

    buildAction(bp)(buildResult)
  }

  def check(v: C): Future[String \/ C] = Future(v.right)

  def filter(f: Request[A] => Future[Boolean]) =
    new Post[A, B, C](bp)(run) {
      override def accept(r: Request[A]) = f(r)
      override def check(v: C) = self.check(v)
    }

  def validate(f: C => Future[String \/ C]) =
    new Post[A, B, C](bp)(run) {
      override def accept(r: Request[A]) = self.accept(r)
      override def check(v: C) = f(v)
    }
}

object Post {
  def apply[A, B, C](bp: BodyParser[A])(run: Request[A] => B => C => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext, p: Pickler[C, A], pc: ParamsCollector[B]) =
    new Post[A, B, C](bp)(run)

  def json[A, B](run: Request[JsValue] => A => B => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext, p: JSONPickler[B], pc: ParamsCollector[A]) =
    new Post[JsValue, A, B](BodyParsers.parse.json)(run)

  def json1[A](run: Request[JsValue] => A => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext, p: JSONPickler[A]) =
    json[Unit, A](r => _ => run(r))
}
