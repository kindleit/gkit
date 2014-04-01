package play.modules.gresource.mongo

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gresource._

object SignOut extends Op {

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = Action(Ok.withNewSession)
}
