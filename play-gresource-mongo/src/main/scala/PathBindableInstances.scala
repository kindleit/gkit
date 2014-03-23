package play.modules.gresource.mongo

import play.api.mvc._

import reactivemongo.bson.BSONObjectID

trait PathBindableInstances {

  implicit def BSONObjectIDPathBindable(implicit binder: PathBindable[String]) =
    new PathBindable[BSONObjectID] {

      def fromString(s: String) =
        try { Right(BSONObjectID(s)) } catch { case e: Throwable => Left(e.getMessage) }

      def bind(key: String, value: String): Either[String, BSONObjectID] =
        binder.bind(key, value).fold(Left(_), fromString)

      def unbind(key: String, boid: BSONObjectID): String =
        boid.stringify
    }
}
