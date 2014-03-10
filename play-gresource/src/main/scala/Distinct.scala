package play.modules.gresource

import gmongo._

import play.modules.gjson._

import play.api.mvc._
import play.core.Router._
import play.core._

import scala.concurrent.Future

import scalaz._

class Distinct[R, Q](cname: String, key: String, query: Q)
  (implicit
    dbe: DbEnv
  , rbp: BSONPickler[R]
  , qbp: BSONPickler[Q]
  , jsp: JSONPickler[R]
  )
    extends Op with Controller {

  import play.modules.gjson.JSON._, BSON._

  implicit val ec = dbe.executionContext

  val method = "GET"

  def mkResponse(r: Future[String \/ R]) =
    Action.async(r.map(_.fold(e => InternalServerError(e), as => Ok(toJSON(as)))))

  def distinct =
    mkResponse(collection(cname).distinct[R](key, query))

  def ap(params: RouteParams) = call(distinct)
}

object Distinct {
  def apply[R] = new Object {
    def apply[Q](cname: String, key: String, query: Q = EmptyQ)
    (implicit
      dbe: DbEnv
    , rbp: BSONPickler[R]
    , bsp: BSONPickler[Q]
    , jsp: JSONPickler[R]
    )
    = new Distinct[R, Q](cname, key, query)
  }
}
