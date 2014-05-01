package play.modules.gresource.mongo

import gkit.mongo._

import play.api.Play.current

import play.api.mvc.Results._
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}

import scalaz._

class Distinct[A, B](cname: String, key: String, query: B)
  (implicit
    ec: ExecutionContext
  , rbp: BSONPickler[A]
  , qbp: BSONPickler[B]
  , jsp: JSONPickler[A]
  ) extends Op[AnyContent] {

  import play.modules.gjson.JSON._

  lazy val route = Route("GET", PathPattern(List(StaticPart(prefix))))

  def executionContext = ec

  def action(rp: RouteParams) = {

    implicit val dbe = GMongoPlugin.dbEnv

    def buildResult(r: Request[AnyContent]) = {
      val f = collection(cname).distinct[A](key, query)
      f.map(_.fold(e => InternalServerError(e), as => Ok(toJSON(as))))
    }

    buildAction(parse.anyContent)(buildResult)
  }

  def filter(f: Request[AnyContent] => Future[Boolean]) =
    new Distinct[A, B](cname, key, query) {
      override def accept(r: Request[AnyContent]) = f(r)
    }
}

object Distinct {
  def apply[A] = new {
    def apply[B](cname: String, key: String, query: B = EmptyQ)
    (implicit
      ec: ExecutionContext
    , rbp: BSONPickler[A]
    , bsp: BSONPickler[B]
    , jsp: JSONPickler[A]
    )
    = new Distinct[A, B](cname, key, query)
  }
}
