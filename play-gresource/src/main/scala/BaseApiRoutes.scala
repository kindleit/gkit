package play.modules.gresource

import play.api.mvc._
import play.core.Router.Routes

import scala.runtime.AbstractPartialFunction

abstract class BaseApiRoutes extends Routes {

  private var path: String = ""

  def subRoutes: List[(String, Routes)]

  def routes: PartialFunction[RequestHeader, Handler] =
    new AbstractPartialFunction[RequestHeader, Handler] {

      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) = {
        if (rh.path.startsWith(path)) {
          val suffix = rh.path.replace(prefix, "")
          subRoutes.find(sr => suffix.startsWith(sr._1)).map(_._2.routes(rh)).getOrElse(Action(Results.BadRequest))
        } else
          Action(Results.BadRequest)
      }

      override def isDefinedAt(rh: RequestHeader) =
        rh.path.startsWith(path)
    }

  override def setPrefix(prefix: String) = {
    subRoutes.foreach { case (suffix, r) => r.setPrefix(prefix + suffix) }
    path = prefix
  }

  override def prefix: String = path

  override def documentation: Seq[(String, String, String)] = Seq()
}
