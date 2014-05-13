package play.modules.gresource

import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.{ExecutionContext, Future}

import scalaz.\/
import scalaz.syntax.either._

class Get[A, B](bp: BodyParser[A])(run: Request[A] => B => Future[String \/ SimpleResult])
  (implicit ec: ExecutionContext, pc: ParamsCollector[B]) extends Op[A] {

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    def buildResult(r: Request[A]) =
      pc.collect(rp).fold(
        e => Future(BadRequest(e)),
        run(r)(_).map(_.fold(InternalServerError(_), identity)))
    buildAction(bp)(buildResult)
  }

  def filter(f: Request[A] => Future[Boolean]) =
    new Get[A, B](bp)(run) {
      override def accept(r: Request[A]) = f(r)
    }
}

object Get {
  def apply(run: Request[AnyContent] => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext) = {
    implicitly[ParamsCollector[Unit]]
    new Get[AnyContent, Unit](BodyParsers.parse.anyContent)(r => _ => run(r))
  }

  def get[A](run: Request[AnyContent] => A => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext, pc: ParamsCollector[A]) =
    new Get[AnyContent, A](BodyParsers.parse.anyContent)(run)

  def withParser[A, B](bp: BodyParser[A])(run: Request[A] => B => Future[String \/ SimpleResult])
    (implicit ec: ExecutionContext , pc: ParamsCollector[B]) =
    new Get[A, B](bp)(run)
}
