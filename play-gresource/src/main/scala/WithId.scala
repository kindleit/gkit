package play.modules.gresource

import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.{Future, ExecutionContext}

import scalaz._
import scalaz.syntax.either._

case class WithId[ID](method: String)(run: Request[AnyContent] => ID => Future[String \/ SimpleResult])
  (implicit
    ec: ExecutionContext
  , pb:  PathBindable[ID]
  ) extends Op[AnyContent] {

  lazy val route =
    Route(method, PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    def buildResult(r: Request[AnyContent]) =
      rp.fromPath[ID]("id").value.fold(
        e => Future(BadRequest(e)),
        id => run(r)(id).map(_.fold(InternalServerError(_), identity)))

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) = new WithId[ID](method)(run) {
    override def accept(r: Request[AnyContent]) = f(r)
  }
}
