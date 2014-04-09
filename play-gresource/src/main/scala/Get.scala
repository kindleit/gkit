package play.modules.gresource

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

case class Get[A](mkResult: (Request[AnyContent], A) => Result)
  (implicit pc: ParamsCollector[A]) extends Op {

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) =
    pc.collect(params).fold(e => Action(BadRequest(e)), v => Action(req => mkResult(req, v)))
}
