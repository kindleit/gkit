package play.modules.gresource

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class Post[A, B](bp: BodyParser[A])(f: A => A)(run: Request[A] => A => B => Future[SimpleResult])
  (implicit ec: ExecutionContext, pc: ParamsCollector[B]) extends Op {

  lazy val route = Route("POST", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) =
    pc.collect(rp).fold(
      e => Action.async(bp)(_ => Future(BadRequest(e))),
      p => Action.async(bp)(r => run(r)(f(r.body))(p)))

  def filter(g: RequestHeader => Future[Boolean]) = new Post[A, B](bp)(f)(run)

  def map(f: A => A) = new Post[A, B](bp)(f)(run)
}

object Post {
  def apply[A](run: Request[AnyContent] => AnyContent => A => Future[SimpleResult])
    (implicit ec: ExecutionContext, pc: ParamsCollector[A]) =
    new Post[AnyContent, A](BodyParsers.parse.anyContent)(identity)(run)

  def apply[A, B](bp: BodyParser[A])(run: Request[A] => A => B => Future[SimpleResult])
    (implicit ec: ExecutionContext , pc: ParamsCollector[B]) =
    new Post[A, B](bp)(identity)(run)
}
