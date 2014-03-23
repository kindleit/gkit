package play.modules.gresource

import play.api.mvc._
import play.api.mvc.Results._

import play.core.Router._
import play.core._

import scala.runtime.AbstractPartialFunction

abstract class Op extends Routes { outer =>

  private var path: String = ""

  def orElse(op: Op): Op = new Op {
    def routes = new AbstractPartialFunction[RequestHeader, Handler] {
      op.setPrefix(prefix)
      outer.setPrefix(prefix)
      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) =
        op.routes.applyOrElse(rh, outer.routes)
      override def isDefinedAt(rh: RequestHeader) =
        op.routes.isDefinedAt(rh) || outer.routes.isDefinedAt(rh)
    }
  }

  def |:(op: Op) = orElse(op)

  def setPrefix(prefix: String) = { path = prefix }
  def prefix: String = path
  def documentation: Seq[(String, String, String)] = Seq()
}
