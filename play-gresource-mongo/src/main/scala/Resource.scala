package play.modules.gresource.mongo

import gkit._

import gkit.mongo._

import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

import scala.concurrent.Future

object Resource {
  def apply[A, ID](cname: String)
    (implicit
      dbe   : DbEnv
    , bsp   : BSONPickler[A]
    , bspj  : BSONProj[A]
    , jsp   : JSONPickler[A]
    , gen   : Generator[ID]
    , idjsp : JSONPickler[ID]
    , idbsp : BSONPickler[ID]
    , idpb  : PathBindable[ID]
    )
    = withFindQuery[A, ID](cname, Find.mkDefaultQry, Find.mkDefaultCntQry)

  def withFindQuery[A, ID] = new {
    def apply[B, C]
      (
        cname: String
      , mkQuery: (Request[AnyContent], Collection, B) => Future[QueryBuilder]
      , mkCountQuery: (Request[AnyContent], B) => Future[C]
      )
      (implicit
        dbe   : DbEnv
      , bsp1  : BSONPickler[A]
      , bsp2  : BSONPickler[C]
      , bspj  : BSONProj[A]
      , jsp   : JSONPickler[A]
      , gen   : Generator[ID]
      , idjsp : JSONPickler[ID]
      , idbsp : BSONPickler[ID]
      , idpb  : PathBindable[ID]
      , pc    : ParamsCollector[B]
      )
      =
      {
        def mkOp(f: RequestHeader => Future[Boolean]) =
          Find[A](cname, mkQuery, mkCountQuery).filter(f) |:
          FindOne[A, ID](cname).filter(f)     |:
          Insert[A, ID](cname).filter(f)      |:
          Update[A, ID](cname).filter(f)      |:
          Delete[ID](cname).filter(f)

        new Op[AnyContent] {
          implicit def executionContext = dbe.executionContext
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
