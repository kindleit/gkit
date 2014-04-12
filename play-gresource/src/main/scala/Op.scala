package play.modules.gresource

import play.api.mvc._

import play.api.http._

import play.core.Router._
import play.core._

import scala.runtime.AbstractPartialFunction

import scala.concurrent.{ExecutionContext, Future}

import scalaz._
import Scalaz._

abstract class Op[A] extends Routes with Results with BodyParsers with Status { self =>

  private var path: String = ""

  implicit def executionContext: ExecutionContext

  def route: Route.ParamsExtractor

  def action(rp: RouteParams): Handler

  def buildAction(bp: BodyParser[A])(f: Request[A] => Future[SimpleResult]): Action[A] =
    Action.async(bp)(r => accept(r).flatMap(_.fold(f(r), Future(Forbidden))))

  def accept(r: Request[A]): Future[Boolean] = Future(true)

  def routes = params(route) >>> (action _).arrow[PartialFunction]

  def params(pe: Route.ParamsExtractor): PartialFunction[RequestHeader, RouteParams] = {
    case pe(rp) => rp
  }

  def orElse[B](op: Op[B]) = new Op[A] {
    def executionContext = self.executionContext
    def route = self.route
    def action(rp: RouteParams) = self.action(rp)
    override def routes = new AbstractPartialFunction[RequestHeader, Handler] {
      op.setPrefix(prefix)
      self.setPrefix(prefix)
      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) =
        op.routes.applyOrElse(rh, self.routes)
      override def isDefinedAt(rh: RequestHeader) =
        op.routes.isDefinedAt(rh) || self.routes.isDefinedAt(rh)
    }
  }

  def |:[B](op: Op[B]) = orElse(op)

  def prefix = path

  def setPrefix(prefix: String) = { path = prefix }

  def documentation = Seq()
}
