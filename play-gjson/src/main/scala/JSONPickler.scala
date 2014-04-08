package play.modules.gjson

import gkit.Pickler

import play.api.libs.json._

import org.joda.time.DateTime

import scala.language.experimental.macros

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import shapeless._
import shapeless.record._

trait JSONPickler[A] extends Pickler[A, JsValue]

object JSONPickler {

  implicit def apply[A](implicit ev: LabelledTypeClass[JSONPickler]): JSONPickler[A] =
    macro GenericMacros.deriveLabelledInstance[JSONPickler, A]

  implicit def StringJSONPickler: JSONPickler[String] = new JSONPickler[String] {
    def pickle(s: String): JsValue = JsString(s)
    def unpickle(v: JsValue, path: List[String]): String \/ String =
      typecheck[JsString](v, path)(_.value)
  }

  implicit def BooleanJSONPickler: JSONPickler[Boolean] = new JSONPickler[Boolean] {
    def pickle(s: Boolean): JsValue = JsBoolean(s)
    def unpickle(v: JsValue, path: List[String]): String \/ Boolean =
      typecheck[JsBoolean](v, path)(_.value)
  }

  implicit def IntJSONPickler: JSONPickler[Int] = new JSONPickler[Int] {
    def pickle(s: Int): JsValue = JsNumber(s)
    def unpickle(v: JsValue, path: List[String]): String \/ Int =
      typecheck[JsNumber](v, path)(_.value.toInt)
  }

  implicit def DoubleJSONPickler: JSONPickler[Double] = new JSONPickler[Double] {
    def pickle(s: Double): JsValue = JsNumber(s)
    def unpickle(v: JsValue, path: List[String]): String \/ Double =
      typecheck[JsNumber](v, path)(_.value.toDouble)
  }

  implicit def DateTimeJSONPickler: JSONPickler[DateTime] = new JSONPickler[DateTime] {
    def pickle(dt: DateTime): JsValue = Json.obj("$date" -> dt.getMillis)
    def unpickle(v: JsValue, path: List[String]): String \/ DateTime = for {
      jso <- typecheck[JsObject](v, path)(identity)
      jss <- (jso \ "$date").asOpt[JsNumber].cata(_.right, s"""type mismatch at `${path.mkString(".")}`: number expected""".left)
    } yield new DateTime(jss.value.toLong)
  }

  implicit def OptionJSONPickler[T](implicit bp: JSONPickler[T]): JSONPickler[Option[T]] =
    new JSONPickler[Option[T]] {
      def pickle(t: Option[T]): JsValue = t.map(bp.pickle).getOrElse(JsNull)
      def unpickle(v: JsValue, path: List[String]): String \/ Option[T] = v match {
        case JsNull => None.right
        case x      => bp.unpickle(x, path).map(Some(_))
      }
    }

  implicit def ListJSONPickler[T](implicit bp: JSONPickler[T]): JSONPickler[List[T]] = new JSONPickler[List[T]] {
    def pickle(t: List[T]): JsValue = JsArray(t.map(bp.pickle))
    def unpickle(v: JsValue, path: List[String]): String \/ List[T] = for {
      ps <- typecheck[JsArray](v, path)(_.value.map(bp.unpickle(_, path)))
      r  <- ps.toList.sequence[({type t[T] = String \/ T})#t, T]
    } yield r
  }

  implicit def HNilJSONPickler: JSONPickler[HNil] =
    new JSONPickler[HNil] {
      def pickle(nil: HNil): JsValue = Json.obj()
      def unpickle(v: JsValue, path: List[String]): String \/ HNil = HNil.right
    }

  implicit def HListJSONPickler[H, T <: HList]
    (implicit hbp: JSONPickler[H], tbp: JSONPickler[T]): JSONPickler[H :: T] =
    new JSONPickler[H :: T] {
      def pickle(l: H :: T): JsValue = {
        val h = Json.arr(hbp.pickle(l.head))
        val t = tbp.pickle(l.tail)
        h ++ (if (t == Json.obj()) Json.arr() else t).asInstanceOf[JsArray]
      }
      def unpickle(v: JsValue, path: List[String]): String \/ (H :: T) = ???
    }

  implicit def RecordJSONPickler[F, V, T <: HList]
    (implicit hjp: JSONPickler[V], tjp: JSONPickler[T], wk: Witness.Aux[F]): JSONPickler[FieldType[F, V] :: T] =
    new JSONPickler[FieldType[F, V] :: T] {
      val name = wk.value match {
        case s: Symbol => s.toString.drop(1)
        case a: Any    => a.toString
      }
      def pickle(l: FieldType[F, V] :: T): JsValue =
        Json.obj(name -> hjp.pickle(l.head:V)) ++ tjp.pickle(l.tail).asInstanceOf[JsObject]
      def unpickle(v: JsValue, path: List[String]): String \/ (FieldType[F, V] :: T) = for {
        o <- typecheck[JsObject](v, path)(identity)
        v <- o.value.get(name).cata(_.right, s"""field `${(path :+ name).mkString(".")}' not found""".left)
        h <- hjp.unpickle(v, path :+ name)
        t <- tjp.unpickle(JsObject(o.fields.filter(_ != (name, v))), path)
      } yield field[F](h) :: t
    }

  implicit def JSONPicklerI: LabelledTypeClass[JSONPickler] = new LabelledTypeClass[JSONPickler] {

    def emptyProduct: JSONPickler[HNil] = new JSONPickler[HNil] {
      def pickle(nil: HNil): JsValue = Json.obj()
      def unpickle(b: JsValue, path: List[String]): String \/ HNil = HNil.right
    }

    def product[H, T <: HList](name: String, JPH: JSONPickler[H], JPT: JSONPickler[T]): JSONPickler[H :: T] =
      new JSONPickler[H :: T] {
        def pickle(l: H :: T): JsValue = {
          val r = Json.obj(name -> JPH.pickle(l.head)) ++ JPT.pickle(l.tail).asInstanceOf[JsObject]
          JsObject(r.fields.filter(_._2 != JsNull))
        }
        def unpickle(v: JsValue, path: List[String]): String \/ (H :: T) = for {
          o <- typecheck[JsObject](v, path)(identity)
          v <- o.value.get(name).cata(_.right, JsNull.right)
          h <- JPH.unpickle(v, path :+ name)
          t <- JPT.unpickle(JsObject(o.fields.filter(_ != (name, v))))
        } yield h :: t
      }

    def emptyCoproduct: JSONPickler[CNil] = new JSONPickler[CNil] {
      def pickle(nil: CNil): JsValue = Json.obj()
      def unpickle(b: JsValue, path: List[String]): String \/ CNil = "".left
    }

    def coproduct[L, R <: Coproduct](name: String, JPL: => JSONPickler[L], JPR: => JSONPickler[R]): JSONPickler[L :+: R] =
      new JSONPickler[L :+: R] {
        def pickle(c: L :+: R): JsValue = c match {
          case Inl(l) => JPL.pickle(l)
          case Inr(r) => JPR.pickle(r)
        }
        def unpickle(v: JsValue, path: List[String]): String \/ (L :+: R) =
          JPR.unpickle(v, path).map(Inr[L, R](_)).orElse(JPL.unpickle(v, path).map(r => Inl[L, R](r)))
      }

    def project[F, G](instance: => JSONPickler[G], to: F => G, from: G => F): JSONPickler[F] =
      new JSONPickler[F] {
        def pickle(f: F): JsValue = instance.pickle(to(f))
        def unpickle(v: JsValue, path: List[String]): String \/ F = instance.unpickle(v, path).map(from)
      }
  }
}
