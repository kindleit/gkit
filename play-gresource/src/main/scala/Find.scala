package play.modules.gresource

import gmongo._

import play.api.mvc._

import play.core._
import play.core.Router._

import play.modules.gjson._

import scala.concurrent.Future

import scalaz._

class Find[A, Q](cname: String, query: Q)
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , bspj: BSONProj[A]
  , qbp: BSONPickler[Q]
  )
    extends Op with Controller {

  import play.modules.gjson.JSON._, BSON._

  implicit val ec = dbe.executionContext

  val method = "GET"

  def mkResponse(r: Future[String \/ List[A]]) =
    Action.async(r.map(_.fold(InternalServerError(_), as => Ok(toJSON(as)))))

  def find(skip: Int, limit: Int) =
    mkResponse(collection(cname).find(query).drop(skip).take(limit).cursor[A].collect[List](upTo = limit))

  def ap(params: RouteParams) =
    call(params.fromQuery[Int]("skip", None), params.fromQuery[Int]("limit", None))(find)
}

object Find {
  def apply[A] = new Object {
    def apply[Q](cname: String, query: Q = EmptyQ)
    (implicit
      dbe: DbEnv
    , bsp: BSONPickler[A]
    , jsp: JSONPickler[A]
    , bspj: BSONProj[A]
    , qbp: BSONPickler[Q]
    )
    = new Find[A, Q](cname, query)
  }
}
