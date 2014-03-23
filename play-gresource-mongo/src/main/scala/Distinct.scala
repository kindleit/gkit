package play.modules.gresource.mongo

import gkit.mongo._

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.Future

import scalaz._

class Distinct[R, Q](cname: String, key: String, query: Q)
  (implicit
    dbe: DbEnv
  , rbp: BSONPickler[R]
  , qbp: BSONPickler[Q]
  , jsp: JSONPickler[R]
  ) extends Op {

  import play.modules.gjson.JSON._, BSON._

  implicit val ec = dbe.executionContext

  lazy val paramsExt = Route("GET", PathPattern(List(StaticPart(prefix))))

  def routes = {
    case paramsExt(_) => call(distinct)
  }

  def mkResponse(r: Future[String \/ R]) =
    Action.async(r.map(_.fold(e => InternalServerError(e), as => Ok(toJSON(as)))))

  def distinct =
    mkResponse(collection(cname).distinct[R](key, query))
}

object Distinct {
  def apply[R] = new {
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
