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
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  implicit val executionContext = dbe.executionContext

  lazy val route =
    Route("GET", PathPattern(List(StaticPart(s"$prefix/"), DynamicPart("id", ".+", false))))

  def action(rp: RouteParams) = {

    def findOne(id: ID) = collection(cname).find(IdQ(id)).one[A]

    def mapToStatus(f: Future[Option[A]]) = f.map(_.cata(a => Ok(toJSON(a)), NotFound))

    def buildResult(r: Request[AnyContent]) =
      rp.fromPath[ID]("id", None).value.fold(e => Future(BadRequest(e)), findOne _ andThen mapToStatus _)

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new FindOne[A, ID](cname) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}
