package play.modules.gresource

import gmongo._

import play.api.libs.json._
import play.api.mvc._

import play.modules.gmongo._

import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.LastError

import scalaz._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

import scala.concurrent.Future

import ResourceCtlr._

class Resource[A, ID]
  (
    cname: String
  , val perms: Map[ResourceOp, Perm] = Map()
  , val projs: Map[ResourceOp, List[Field]] = Map()
  )
  (implicit
    dbe: DbEnv
  , bsp: BSONPickler[A]
  , jsp: JSONPickler[A]
  , idp: BSONPickler[ID]
  , pb:  PathBindable[ID]
  )
    extends ResourceRoutes[ID] with ResourceCtlr[ID] {

  import play.modules.gmongo.JSON._

  implicit val ec = dbe.executionContext

  val coll = collection(cname)

  def findOneById(id: ID) = coll.find(IdQ(id)).one[A]

  def removeOneById(id: ID) = coll.remove(IdQ(id))

  def retrievePage = coll.find().cursor[A].collect[List]()

  def doInsert(a: A) = coll.insert(a)

  def doUpdate(id: ID, a: A) = coll.update(IdQ(id), Set(a))

  def mkResponse(le: LastError) =
    le.ok.fold(Ok, InternalServerError(~le.errMsg))

  def fromRequest(req: Request[JsValue]): String \/ A =
    req.body.asOpt[JsObject].cata(fromJSON[A], "invalid payload: Json object expected".left[A])

  override def find =
    Action.async(retrievePage.map(_.fold(InternalServerError(_), as => Ok(toJSON(as)))))

  override def insert = Action.async(parse.json) { req =>
    fromRequest(req).fold(e => Future(BadRequest(e)), a => doInsert(a).map(mkResponse))
  }

  override def get(id: ID) =
    Action.async(findOneById(id).map(_.cata(a => Ok(toJSON(a)), NotFound)))

  override def update(id: ID) = Action.async(parse.json) { req =>
    fromRequest(req).fold(e => Future(BadRequest(e)), a => doUpdate(id, a).map(mkResponse))
  }

  override def delete(id: ID) =
    Action.async(removeOneById(id).map(_.updatedExisting.fold(Ok, NotFound)))
}

object Resource {

  def apply[A, ID]
    (
      cname: String
    , perms: Map[ResourceOp, Perm] = Map()
    , projs: Map[ResourceOp, List[Field]] = Map()
    )
    (implicit
      dbe: DbEnv
    , bsp: BSONPickler[A]
    , jsp: JSONPickler[A]
    , idp: BSONPickler[ID]
    , pb:  PathBindable[ID]
    )
    = new Resource[A, ID](cname, perms, projs)

  implicit def boidPathBindable(implicit binder: PathBindable[String]) =
    new PathBindable[BSONObjectID] {

      def fromString(s: String) =
        try { Right(BSONObjectID(s)) } catch { case e: Throwable => Left(e.getMessage) }

      def bind(key: String, value: String): Either[String, BSONObjectID] =
        binder.bind(key, value).fold(Left(_), fromString)

      def unbind(key: String, boid: BSONObjectID): String =
        boid.stringify
    }
}
