package play.modules.gresource.mongo

import gkit.mongo._

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gresource._

import scala.concurrent.Future

import scalaz.syntax.std.boolean._

case class Delete[ID](cname: String)
  (implicit
    dbe: DbEnv
  , idp: BSONPickler[ID]
  , pb:  PathBindable[ID]
  ) extends Op[AnyContent] {

  implicit val executionContext = dbe.executionContext

  lazy val route =
    Route("DELETE", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def action(rp: RouteParams) = {

    def delete(id: ID) =
      collection(cname).remove(IdQ(id)).map(le => (le.updated > 0).fold(Ok, NotFound))

    def buildResult(r: Request[AnyContent]) =
      rp.fromPath[ID]("id").value.fold(e => Future(BadRequest(e)), delete)

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) = new Delete[ID](cname) {
    override def accept(r: Request[AnyContent]) = f(r)
  }
}
