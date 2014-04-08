package play.modules.gresource.mongo

import play.api.libs.json._

import play.modules.gjson._

import reactivemongo.bson.BSONObjectID

import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.std.option._

trait PicklerInstances {

  implicit def BSONObjectIDPickler: JSONPickler[BSONObjectID] =
    new JSONPickler[BSONObjectID] {
      def pickle(boid: BSONObjectID): JsValue = Json.obj("$oid" -> boid.stringify)
      def unpickle(v: JsValue, path: List[String]): String \/ BSONObjectID = for {
        jso <- typecheck[JsObject](v, path)(identity)
        jss <- (jso \ "$oid").asOpt[JsString].cata(_.right, "string expected".left)
      } yield BSONObjectID(jss.value)
    }
}
