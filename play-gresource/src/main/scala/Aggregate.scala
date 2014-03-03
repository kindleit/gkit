package play.modules.gresource

import gmongo._

import play.modules.gmongo._

import play.api.mvc._
import play.core.Router._
import play.core._

import scala.concurrent.Future

import scalaz._

import shapeless._

class Aggregate[R, P <: HList](cname: String, pipeline: P)
  (implicit
    dbe: DbEnv
  , rbp: BSONPickler[R]
  , qbp: BSONPickler[P]
  , jsp: JSONPickler[R]
  )
    extends Op with Controller {

  import play.modules.gmongo.JSON._, BSON._

  implicit val ec = dbe.executionContext

  val method = "GET"

  def mkResponse(r: Future[String \/ R]) =
    Action.async(r.map(_.fold(e => InternalServerError(e), as => Ok(toJSON(as)))))

  def aggregate =
    mkResponse(collection(cname).aggregate[R](pipeline))

  def ap(params: RouteParams) = call(aggregate)
}

object Aggregate {
  def apply[R] = new Object {
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
