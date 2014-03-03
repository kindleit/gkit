package play.modules.gmongo

import gmongo.Pickler

import play.api.libs.json._

import org.joda.time.DateTime

import reactivemongo.bson.BSONObjectID

import scala.language.experimental.macros

import scalaz._
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import shapeless._

trait JSONPickler[A] extends Pickler[A, JsValue]

object JSONPickler {

  implicit def apply[T]: JSONPickler[T] = macro TypeClass.derive_impl[JSONPickler, T]

  implicit def StringJSONPickler: JSONPickler[String] = new JSONPickler[String] {
    def pickle(s: String): JsValue = JsString(s)
    def unpickle(v: JsValue): String \/ String = typecheck[JsString](v, _.value)
  }

  implicit def BooleanJSONPickler: JSONPickler[Boolean] = new JSONPickler[Boolean] {
    def pickle(s: Boolean): JsValue = JsBoolean(s)
    def unpickle(v: JsValue): String \/ Boolean = typecheck[JsBoolean](v, _.value)
  }

  implicit def IntJSONPickler: JSONPickler[Int] = new JSONPickler[Int] {
    def pickle(s: Int): JsValue = JsNumber(s)
    def unpickle(v: JsValue): String \/ Int = typecheck[JsNumber](v, _.value.toInt)
  }

  implicit def DoubleJSONPickler: JSONPickler[Double] = new JSONPickler[Double] {
    def pickle(s: Double): JsValue = JsNumber(s)
    def unpickle(v: JsValue): String \/ Double = typecheck[JsNumber](v, _.value.toDouble)
  }

  implicit def DateTimeJSONPickler: JSONPickler[DateTime] = new JSONPickler[DateTime] {
    def pickle(dt: DateTime): JsValue = Json.obj("$date" -> dt.getMillis)
    def unpickle(v: JsValue): String \/ DateTime = for {
      jso <- typecheck[JsObject](v, x => x)
      jss <- (jso \ "$date").asOpt[JsNumber].cata(_.right, "number expected".left)
    } yield new DateTime(jss.value)
  }

  implicit def BSONObjectIDPickler: JSONPickler[BSONObjectID] = new JSONPickler[BSONObjectID] {
    def pickle(boid: BSONObjectID): JsValue = Json.obj("$oid" -> boid.stringify)
    def unpickle(v: JsValue): String \/ BSONObjectID = for {
      jso <- typecheck[JsObject](v, x => x)
      jss <- (jso \ "$oid").asOpt[JsString].cata(_.right, "string expected".left)
    } yield BSONObjectID(jss.value)
  }

  implicit def OptionJSONPickler[T](implicit bp: JSONPickler[T]): JSONPickler[Option[T]] = new JSONPickler[Option[T]] {
    def pickle(t: Option[T]): JsValue = t.map(bp.pickle).getOrElse(JsNull)
    def unpickle(v: JsValue): String \/ Option[T] = bp.unpickle(v).map(Some(_))
  }

  implicit def ListJSONPickler[T](implicit bp: JSONPickler[T]): JSONPickler[List[T]] = new JSONPickler[List[T]] {
    def pickle(t: List[T]): JsValue = JsArray(t.map(bp.pickle))
    def unpickle(v: JsValue): String \/ List[T] = for {
      ps <- typecheck[JsArray](v, _.value.map(bp.unpickle))
      r  <- ps.toList.sequence[({type t[T] = String \/ T})#t, T]
    } yield r
  }

  implicit def JSONPicklerI: ProductTypeClass[JSONPickler] = new ProductTypeClass[JSONPickler] {

    def emptyProduct: JSONPickler[HNil] = new JSONPickler[HNil] {
      def pickle(nil: HNil) = Json.obj()
      def unpickle(b: JsValue) = HNil.right
    }

    def product[H, T <: HList](head: JSONPickler[H], tail: JSONPickler[T]): JSONPickler[H :: T] =
      new JSONPickler[H :: T] {
        def pickle(l: H :: T): JsValue = ???
        def unpickle(v: JsValue): String \/ (H :: T) = ???
      }

    override def namedProduct[H, T <: HList](head: JSONPickler[H], name: String, tail: JSONPickler[T]): JSONPickler[H :: T] =
      new JSONPickler[H :: T] {
        def pickle(l: H :: T): JsValue = {
          val r = Json.obj(name -> head.pickle(l.head)) ++ tail.pickle(l.tail).asInstanceOf[JsObject]
          JsObject(r.fields.filter(_._2 != JsNull))
        }
        def unpickle(v: JsValue): String \/ (H :: T) = for {
          o <- typecheck[JsObject](v, x => x)
          v <- o.value.get(name).cata(_.right, s"field `$name' not found".left)
          h <- head.unpickle(v)
          t <- tail.unpickle(JsObject(o.fields.filter(_ != (name, v))))
        } yield h :: t
      }

    def project[F, G](instance: => JSONPickler[G], to: F => G, from: G => F): JSONPickler[F] = new JSONPickler[F] {
      def pickle(f: F): JsValue = instance.pickle(to(f))
      def unpickle(v: JsValue): String \/ F = instance.unpickle(v).map(from)
    }
  }
}
