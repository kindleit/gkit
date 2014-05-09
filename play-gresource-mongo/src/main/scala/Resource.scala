package play.modules.gresource.mongo

import gkit._

import gkit.mongo._

import play.api.Play.current
import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.{Future, ExecutionContext}

import scalaz.\/

object Resource {
  def apply[A, ID](cname: String)
    (implicit
      ec  : ExecutionContext
    , bp1 : BSONPickler[A]
    , bp2 : BSONPickler[ID]
    , jp1 : JSONPickler[A]
    , jp2 : JSONPickler[ID]
    , gen : Generator[ID]
    , pb  : PathBindable[ID]
    )
    = withFindQuery[A, ID](cname, Find.mkDefaultQry[A])

  def withFindQuery[A, ID] = new WithFindQuery[A, ID]

  class WithFindQuery[A, ID] {
    def apply[B, C]
      (
        cname: String
      , mkQuery: (Request[AnyContent], B, Collection) => Future[String \/ C]
      )
      (implicit
        ec  : ExecutionContext
      , bp1 : BSONPickler[A]
      , bp2 : BSONPickler[ID]
      , jp1 : JSONPickler[A]
      , jp2 : JSONPickler[C]
      , jp3 : JSONPickler[ID]
      , gen : Generator[ID]
      , pb  : PathBindable[ID]
      , pc  : ParamsCollector[B]
      )
      =
      {
        def mkOp(f: RequestHeader => Future[Boolean]) =
          Find(cname, mkQuery).filter(f) |:
          FindOne[A, ID](cname).filter(f)   |:
          Insert[A, ID](cname).filter(f)    |:
          Update[A, ID](cname).filter(f)    |:
          Delete[ID](cname).filter(f)

        new Op[AnyContent] {
          def executionContext = ec
          val op = mkOp(_ => Future(true))
          val route = op.route
          def action(rp: RouteParams) = op.action(rp)
          def filter(f: RequestHeader => Future[Boolean]) = mkOp(f)
          override def routes = op.routes
          override def accept(r: Request[AnyContent]) = op.accept(r)
        }
      }
  }
}
