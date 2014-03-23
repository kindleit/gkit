package gkit.mongo

import gkit.Pickler

import org.joda.time.DateTime

import reactivemongo.bson._

import scala.language.experimental.macros

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import shapeless._
import shapeless.record._

trait BSONPickler[A] extends Pickler[A, BSONValue]

object BSONPickler {

  implicit def apply[A]: BSONPickler[A] = macro TypeClass.derive_impl[BSONPickler, A]

  implicit def StringBSONPickler: BSONPickler[String] = new BSONPickler[String] {
    def pickle(s: String): BSONValue = BSONString(s)
    def unpickle(v: BSONValue): String \/ String = typecheck[BSONString](v, _.value)
  }

  implicit def BooleanBSONPickler: BSONPickler[Boolean] = new BSONPickler[Boolean] {
    def pickle(s: Boolean): BSONValue = BSONBoolean(s)
    def unpickle(v: BSONValue): String \/ Boolean = typecheck[BSONBoolean](v, _.value)
  }

  implicit def DoubleBSONPickler: BSONPickler[Double] = new BSONPickler[Double] {
    def pickle(s: Double): BSONValue = BSONDouble(s)
    def unpickle(v: BSONValue): String \/ Double = typecheck[BSONDouble](v, _.value)
  }

  implicit def IntBSONPickler: BSONPickler[Int] = new BSONPickler[Int] {
    def pickle(s: Int): BSONValue = BSONInteger(s)
    def unpickle(v: BSONValue): String \/ Int = typecheck[BSONInteger](v, _.value)
  }

  implicit def DateTimeBSONPickler: BSONPickler[DateTime] = new BSONPickler[DateTime] {
    def pickle(dt: DateTime): BSONValue = BSONDateTime(dt.getMillis)
    def unpickle(v: BSONValue): String \/ DateTime = typecheck[BSONDateTime](v, dt => new DateTime(dt.value))
  }

  implicit def BSONObjectIDPickler: BSONPickler[BSONObjectID] = new BSONPickler[BSONObjectID] {
    def pickle(boid: BSONObjectID): BSONValue = boid
    def unpickle(v: BSONValue): String \/ BSONObjectID = typecheck[BSONObjectID](v, x => x)
  }

  implicit def BSONDocumentPickler: BSONPickler[BSONDocument] = new BSONPickler[BSONDocument] {
    def pickle(doc: BSONDocument): BSONValue = doc
    def unpickle(v: BSONValue): String \/ BSONDocument = typecheck[BSONDocument](v, x => x)
  }

  implicit def OptionBSONPickler[T](implicit bp: BSONPickler[T]): BSONPickler[Option[T]] = new BSONPickler[Option[T]] {
    def pickle(t: Option[T]): BSONValue = t.map(bp.pickle).getOrElse(BSONUndefined)
    def unpickle(v: BSONValue): String \/ Option[T] =
      if (v == BSONUndefined || v == BSONNull) None.right
      else bp.unpickle(v).map(Some(_))
  }

  implicit def ListBSONPickler[T](implicit bp: BSONPickler[T]): BSONPickler[List[T]] = new BSONPickler[List[T]] {
    def pickle(t: List[T]): BSONValue = BSONArray(t.map(bp.pickle))
    def unpickle(v: BSONValue): String \/ List[T] = for {
      ps <- typecheck[BSONArray](v, _.values.map(bp.unpickle))
      r  <- ps.toList.sequence[({type t[T] = String \/ T})#t, T]
    } yield r
  }

  implicit def HNilBSONPickler: BSONPickler[HNil] =
    new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONDocument()
      def unpickle(v: BSONValue): String \/ HNil = HNil.right
    }

  implicit def HListBSONPickler[H, T <: HList]
    (implicit hbp: BSONPickler[H], tbp: BSONPickler[T]): BSONPickler[H :: T] =
    new BSONPickler[H :: T] {
      def pickle(l: H :: T): BSONValue = {
        val h = BSONArray(hbp.pickle(l.head))
        val t = tbp.pickle(l.tail)
        h ++ (if (t == BSONDocument()) BSONArray() else t).asInstanceOf[BSONArray]
      }
      def unpickle(v: BSONValue): String \/ (H :: T) = ???
    }

  implicit def RecordBSONPickler[F, V, T <: HList]
    (implicit hbp: BSONPickler[V], tbp: BSONPickler[T], wk: Witness.Aux[F]): BSONPickler[FieldType[F, V] :: T] =
    new BSONPickler[FieldType[F, V] :: T] {
      def pickle(l: FieldType[F, V] :: T): BSONValue =
        BSONDocument(wk.value.toString -> hbp.pickle(l.head:V)) ++ tbp.pickle(l.tail).asInstanceOf[BSONDocument]
      def unpickle(v: BSONValue): String \/ (FieldType[F, V] :: T) = ???
    }

  implicit def BSONPicklerI: TypeClass[BSONPickler] = new TypeClass[BSONPickler] {

    def emptyProduct: BSONPickler[HNil] = new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONDocument()
      def unpickle(b: BSONValue): String \/ HNil = HNil.right
    }

    def product[H, T <: HList](head: BSONPickler[H], tail: BSONPickler[T]): BSONPickler[H :: T] =
      new BSONPickler[H :: T] {
        def pickle(l: H :: T): BSONValue = ???
        def unpickle(v: BSONValue): String \/ (H :: T) = ???
      }

    override def namedProduct[H, T <: HList](head: BSONPickler[H], name: String, tail: BSONPickler[T]): BSONPickler[H :: T] =
      new BSONPickler[H :: T] {
        def pickle(l: H :: T): BSONValue = {
          val r = BSONDocument(name -> head.pickle(l.head)) ++ tail.pickle(l.tail).asInstanceOf[BSONDocument]
          BSONDocument(r.elements.filter(_._2 != BSONUndefined))
        }
        def unpickle(v: BSONValue): String \/ (H :: T) = for {
          d <- typecheck[BSONDocument](v, x => x)
          v <- d.get(name).cata(_.right, BSONUndefined.right)
          h <- head.unpickle(v)
          t <- tail.unpickle(BSONDocument(d.elements.filter(_ != (name, v))))
        } yield h :: t
      }

    override def namedField[F](instance: BSONPickler[F], name: String): BSONPickler[F] = new BSONPickler[F] {
      def pickle(f: F): BSONValue = BSONDocument(name -> instance.pickle(f))
      def unpickle(v: BSONValue): String \/ F = for {
        d <- typecheck[BSONDocument](v, x => x)
        v <- d.get(name).cata(_.right, BSONUndefined.right)
        r <- instance.unpickle(v)
      } yield r
    }

    def coproduct[L, R <: Coproduct](BL: => BSONPickler[L], BR: => BSONPickler[R]): BSONPickler[L :+: R] =
      new BSONPickler[L :+: R] {
        def pickle(c: L :+: R): BSONValue = c match {
          case Inl(l) => BL.pickle(l)
          case Inr(r) => BR.pickle(r)
        }
        def unpickle(v: BSONValue): String \/ (L :+: R) =
          BL.unpickle(v).map(Inl[L, R](_)).orElse(BR.unpickle(v).map(r => Inr[L, R](r)))
      }

    def project[F, G](instance: => BSONPickler[G], to: F => G, from: G => F): BSONPickler[F] = new BSONPickler[F] {
      def pickle(f: F): BSONValue = instance.pickle(to(f))
      def unpickle(v: BSONValue): String \/ F = instance.unpickle(v).map(from)
    }
  }
}
