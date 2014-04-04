package play.modules.gresource.mongo

import gkit._

import gkit.mongo._

import play.api.mvc._

import play.core.Router._
import play.core._

import play.modules.gjson._
import play.modules.gresource._

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
    = withFindQuery[A, ID](cname, Find.mkDefaultQry)

  def withFindQuery[A, ID] = new {
    def apply[B](cname: String, findQuery: (B, Collection) => QueryBuilder)
      (implicit
        dbe   : DbEnv
      , bsp   : BSONPickler[A]
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
        def mkOp(f: RequestHeader => Boolean) =
          Find[A](cname, findQuery).filter(f) |:
          FindOne[A, ID](cname).filter(f)     |:
          Insert[A, ID](cname).filter(f)      |:
          Update[A, ID](cname).filter(f)      |:
          Delete[ID](cname).filter(f)

        new Op {
          val op = mkOp(_ => true)
          val route = op.route
          def mkResponse(params: RouteParams) = op.mkResponse(params)
          def filter(f: RequestHeader => Boolean) = mkOp(f)
          override def routes = op.routes
          override def _filter = op._filter
        }
      }
  }
}
