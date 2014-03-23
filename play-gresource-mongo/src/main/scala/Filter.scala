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

class Filter[A, Q](cname: String, query: Q)
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , bspj: BSONProj[A]
  , qbp: BSONPickler[Q]
  ) extends Op {

  import play.modules.gjson.JSON._, BSON._

  implicit val ec = dbe.executionContext

  lazy val paramsExt = Route("GET", PathPattern(List(StaticPart(prefix))))

  def routes = {
    case paramsExt(params) =>
      call(params.fromQuery[Int]("skip", Some(0)), params.fromQuery[Int]("limit", Some(10)))(find)
  }

  def mkResponse(r: Future[String \/ List[A]]) =
    Action.async(r.map(_.fold(InternalServerError(_), as => Ok(toJSON(as)))))

  def find(skip: Int, limit: Int) =
    mkResponse(collection(cname).find(query).drop(skip).take(limit).cursor[A].collect[List](upTo = limit))
}

object Filter {
  def apply[A] = new {
    def apply[Q](cname: String, query: Q = EmptyQ)
    (implicit
      dbe: DbEnv
    , bsp: BSONPickler[A]
    , jsp: JSONPickler[A]
    , bspj: BSONProj[A]
    , qbp: BSONPickler[Q]
    )
    = new Filter[A, Q](cname, query)
  }
}
