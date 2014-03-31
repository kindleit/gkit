package play.modules.gresource.mongo

import gkit.mongo._

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.Future

import scalaz._
import scalaz.syntax.std.option._

case class FindOne[A, ID](cname: String)
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , idbp: BSONPickler[ID]
  , idpb: PathBindable[ID]
  ) extends Op {

  import play.modules.gjson.JSON._, BSON._

  implicit val ec = dbe.executionContext

  lazy val route =
    Route("GET", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def mkResponse(params: RouteParams) = call(params.fromPath[ID]("id", None))(findOne)

  def findOne(id: ID) = mkAction(collection(cname).find(IdQ(id)).one[A])

  def mkAction(r: Future[Option[A]]) = Action.async(r.map(_.cata(a => Ok(toJSON(a)), NotFound)))
}
