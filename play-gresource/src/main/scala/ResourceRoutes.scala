package play.modules.gresource

import play.api.mvc._
import play.core.Router.Routes

import scala.runtime.AbstractPartialFunction

abstract class ResourceRoutes[T](implicit idBindable: PathBindable[T])
    extends Routes {

  this: ResourceCtlr[T] =>

  private var path: String = ""

  private val Id = "/([^/]+)".r
  private val MaybeSlash = "/?".r

  import ResourceCtlr._

  def routes: PartialFunction[RequestHeader, Handler] =
    new AbstractPartialFunction[RequestHeader, Handler] {

      def withId(id: String, action: T => EssentialAction) =
        idBindable.bind("id", id).fold(badRequest, action)

      def withOp(ro: ResourceOp, rh: RequestHeader, action: => EssentialAction): EssentialAction =
        if (perms.get(ro).getOrElse(Anonymous)(rh)) action
        else Action(Unauthorized)

      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) = {
        if (rh.path.startsWith(path))
          (rh.method, rh.path.drop(path.length)) match {
            case ("GET",    MaybeSlash()) => withOp(Find, rh, find)
            case ("POST",   MaybeSlash()) => withOp(Insert, rh, insert)
            case ("POST",   Id(id))       => withOp(Update, rh, withId(id, update))
            case ("GET",    Id(id))       => withOp(Get, rh, withId(id, get))
            case ("DELETE", Id(id))       => withOp(Delete, rh, withId(id, delete))
            case _                        => Action(BadRequest)
          }
          else
            Action(BadRequest)
      }

      def isDefinedAt(rh: RequestHeader) =
        if (rh.path.startsWith(path))
          (rh.method, rh.path.drop(path.length)) match {
            case ("GET",    MaybeSlash()) => true
            case ("POST",   MaybeSlash()) => true
            case ("POST",   Id(_))        => true
            case ("GET",    Id(_))        => true
            case ("DELETE", Id(_))        => true
            case _                        => false
          }
          else
            false
    }

  def setPrefix(prefix: String) = { path = prefix }

  def prefix: String = path

  def documentation: Seq[(String, String, String)] = Seq()
}
