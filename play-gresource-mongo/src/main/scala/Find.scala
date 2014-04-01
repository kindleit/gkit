package play.modules.gresource.mongo

import gkit.mongo._

import org.joda.time.DateTime

import play.api.mvc._
import play.api.mvc.Results._

import play.core._
import play.core.Router._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.Future

import scalaz._
import scalaz.syntax.std.option._

class Find[A, B](cname: String, mkQuery: (B, Collection) => QueryBuilder)
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , bspj: BSONProj[A]
  , pc: ParamsCollector[B]
  ) extends Op {

  import play.modules.gjson.JSON._

  implicit val ec = dbe.executionContext

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def mkResponse(params: RouteParams) = find(params)

  def find(params: RouteParams) =
    pc.collect(params).fold(e => Action(BadRequest(e)),
      p => mkAction(mkQuery(p, collection(cname)).cursor[A].collect[List]()))

  def mkAction(r: Future[String \/ List[A]]) =
    Action.async(r.map(_.fold(InternalServerError(_), as => Ok(toJSON(as)))))
}

object Find {
  def apply[A] = new {
    def apply[B](cname: String, mkQuery: (B, Collection) => QueryBuilder)
      (implicit
        dbe: DbEnv
      , bsp: BSONPickler[A]
      , jsp: JSONPickler[A]
      , bspj: BSONProj[A]
      , pc: ParamsCollector[B]
      )
      = new Find[A, B](cname, mkQuery)
  }

  case class DefaultParams(skip: Option[Int], limit: Option[Int])

  def mkDefaultQry(implicit dbe: DbEnv) =
    (dp: DefaultParams, c: Collection) => c.find(EmptyQ).drop(dp.skip | 0).take(dp.limit | 10)
}
