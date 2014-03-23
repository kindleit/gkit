package play.modules.gresource.mongo

import gkit._

import gkit.mongo._

import play.api.mvc._

import play.modules.gjson._

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
    = Filter[A](cname, EmptyQ) |:
      Filter1[A, ID](cname)    |:
      Insert[A, ID](cname)     |:
      Update[A, ID](cname)     |:
      Delete[ID](cname)
}
