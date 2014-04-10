package play.modules.gresource

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class Get[A, B](bp: BodyParser[A])(run: Request[A] => B => Future[SimpleResult])
  (implicit ec: ExecutionContext, pc: ParamsCollector[B]) extends Op {

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) =
    pc.collect(params).fold(
      e => Action.async(bp)(_ => Future(BadRequest(e))),
      p => Action.async(bp)(run(_)(p)))
}

object Get {
  def apply(run: Request[AnyContent] => Future[SimpleResult])
    (implicit ec: ExecutionContext) = {
    implicitly[ParamsCollector[Unit]]
    new Get[AnyContent, Unit](BodyParsers.parse.anyContent)(r => _ => run(r))
  }

  def get[A](run: Request[AnyContent] => A => Future[SimpleResult])
    (implicit ec: ExecutionContext, pc: ParamsCollector[A]) =
    new Get[AnyContent, A](BodyParsers.parse.anyContent)(run)

  def withParser[A, B](bp: BodyParser[A])(run: Request[A] => B => Future[SimpleResult])
    (implicit ec: ExecutionContext , pc: ParamsCollector[B]) =
    new Get[A, B](bp)(run)
}
