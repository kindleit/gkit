package play.modules.gresource

import play.api.mvc._
import play.api.mvc.Results._

import play.core.Router._
import play.core._

import scala.runtime.AbstractPartialFunction

abstract class Op extends Routes {

  private var path: String = ""

  val method: String

  lazy val paramsExtractor = Route(method, PathPattern(List(StaticPart(prefix))))

  def ap(params: RouteParams): Handler

  def routes: PartialFunction[RequestHeader, Handler] = {
    case paramsExtractor(params) => ap(params)
  }

  def setPrefix(prefix: String) = { path = prefix }
  def prefix: String = path
  def documentation: Seq[(String, String, String)] = Seq()
}
