package play.modules.gresource.mongo

import gkit.mongo._

import play.modules.gjson._
import play.modules.gresource._

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import scala.concurrent.Future

import scalaz._

import shapeless._

class Aggregate[R, P <: HList](cname: String, pipeline: P)
  (implicit
    dbe: DbEnv
  , rbp: BSONPickler[R]
  , qbp: BSONPickler[P]
  , jsp: JSONPickler[R]
  ) extends Op {

  import play.modules.gjson.JSON._

  implicit val ec = dbe.executionContext

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = aggregate

  def filter(f: RequestHeader => Boolean) = new Aggregate[R, P](cname, pipeline) {
    override def _filter = { case rh => f(rh) }
  }

  def aggregate =
    mkAction(collection(cname).aggregate[R](pipeline))

  def mkAction(r: Future[String \/ R]) =
    Action.async(r.map(_.fold(e => InternalServerError(e), as => Ok(toJSON(as)))))
}

object Aggregate {
  def apply[R] = new {
    def apply[P <: HList](cname: String, pipeline: P)
    (implicit
      dbe: DbEnv
    , rbp: BSONPickler[R]
    , bsp: BSONPickler[P]
    , jsp: JSONPickler[R]
    )
    = new Aggregate[R, P](cname, pipeline)
  }
}
