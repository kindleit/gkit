package play.modules.gresource.mongo

import gkit.mongo._

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gresource._

import scalaz.syntax.std.boolean._

case class Delete[ID](cname: String)
  (implicit
    dbe: DbEnv
  , idp: BSONPickler[ID]
  , pb:  PathBindable[ID]
  ) extends Op {

  implicit val ec = dbe.executionContext

  lazy val route =
    Route("DELETE", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def mkResponse(params: RouteParams) =
    call(params.fromPath[ID]("id"))(delete)

  def delete(id: ID) =
    Action.async(collection(cname).remove(IdQ(id)).map(le => (le.updated > 0).fold(Ok, NotFound)))
}
