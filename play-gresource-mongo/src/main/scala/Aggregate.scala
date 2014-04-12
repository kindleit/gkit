package play.modules.gresource.mongo

import gkit.mongo._

import play.modules.gjson._
import play.modules.gresource._

import play.api.mvc._

import play.core.Router._
import play.core._

import scala.concurrent.Future

import scalaz._

import shapeless._

class Aggregate[A, B <: HList](cname: String, pipeline: B)
  (implicit
    dbe: DbEnv
  , rbp: BSONPickler[A]
  , qbp: BSONPickler[B]
  , jsp: JSONPickler[A]
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  implicit val executionContext = dbe.executionContext

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def action(rp: RouteParams) = {

    def buildResult(r: Request[AnyContent]) = {
      val f = collection(cname).aggregate[A](pipeline)
      f.map(_.fold(InternalServerError(_), as => Ok(toJSON(as))))
    }

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Aggregate[A, B](cname, pipeline) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Aggregate {
  def apply[A] = new {
    def apply[B <: HList](cname: String, pipeline: B)
    (implicit
      dbe: DbEnv
    , rbp: BSONPickler[A]
    , bsp: BSONPickler[B]
    , jsp: JSONPickler[A]
    )
    = new Aggregate[A, B](cname, pipeline)
  }
}
