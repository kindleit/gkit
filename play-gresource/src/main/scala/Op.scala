package play.modules.gresource

import play.api.mvc._
import play.api.mvc.Results._

import play.core.Router._
import play.core._

import scala.runtime.AbstractPartialFunction

import scalaz.std.partialFunction._
import scalaz.syntax.arrow._

abstract class Op extends Routes { self =>

  private var path: String = ""

  def route: Route.ParamsExtractor

  def mkResponse(params: RouteParams): Handler

  def handle: PartialFunction[(RouteParams, Boolean), Handler] = {
    case (params, true) => mkResponse(params)
    case (_, false)     => Action(Forbidden)
  }

  def routes = (extractParams(route) &&& _filter) >>> handle

  def extractParams(pe: Route.ParamsExtractor): PartialFunction[RequestHeader, RouteParams] = {
    case pe(params) => params
  }

  def _filter: PartialFunction[RequestHeader, Boolean] = { case _ => true }

  def orElse(op: Op) = new Op {
    def route = self.route
    def mkResponse(params: RouteParams) = self.mkResponse(params)
    override def routes = new AbstractPartialFunction[RequestHeader, Handler] {
      op.setPrefix(prefix)
      self.setPrefix(prefix)
      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) =
        op.routes.applyOrElse(rh, self.routes)
      override def isDefinedAt(rh: RequestHeader) =
        op.routes.isDefinedAt(rh) || self.routes.isDefinedAt(rh)
    }
  }

  def |:(op: Op) = orElse(op)

  def prefix = path

  def setPrefix(prefix: String) = { path = prefix }

  def documentation = Seq()
}
