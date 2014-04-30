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

  implicit def apply[A](implicit ev: LabelledTypeClass[BSONPickler]): BSONPickler[A] =
    macro GenericMacros.deriveLabelledInstance[BSONPickler, A]

  implicit def StringBSONPickler: BSONPickler[String] = new BSONPickler[String] {
    def pickle(s: String): BSONValue = BSONString(s)
    def unpickle(v: BSONValue, path: List[String]): String \/ String =
      typecheck[BSONString](v, path)(_.value)
  }

  implicit def BooleanBSONPickler: BSONPickler[Boolean] = new BSONPickler[Boolean] {
    def pickle(s: Boolean): BSONValue = BSONBoolean(s)
    def unpickle(v: BSONValue, path: List[String]): String \/ Boolean =
      typecheck[BSONBoolean](v, path)(_.value)
  }

  implicit def DoubleBSONPickler: BSONPickler[Double] = new BSONPickler[Double] {
    def pickle(s: Double): BSONValue = BSONDouble(s)
    def unpickle(v: BSONValue, path: List[String]): String \/ Double =
      typecheck[BSONDouble](v, path)(_.value)
  }

  implicit def IntBSONPickler: BSONPickler[Int] = new BSONPickler[Int] {
    def pickle(s: Int): BSONValue = BSONInteger(s)
    def unpickle(v: BSONValue, path: List[String]): String \/ Int =
      typecheck[BSONInteger](v, path)(_.value)
  }

  implicit def DateTimeBSONPickler: BSONPickler[DateTime] = new BSONPickler[DateTime] {
    def pickle(dt: DateTime): BSONValue = BSONDateTime(dt.getMillis)
    def unpickle(v: BSONValue, path: List[String]): String \/ DateTime =
      typecheck[BSONDateTime](v, path)(dt => new DateTime(dt.value))
  }

  implicit def BSONObjectIDPickler: BSONPickler[BSONObjectID] = new BSONPickler[BSONObjectID] {
    def pickle(boid: BSONObjectID): BSONValue = boid
    def unpickle(v: BSONValue, path: List[String]): String \/ BSONObjectID =
      typecheck[BSONObjectID](v, path)(identity)
  }

  implicit def BSONDocumentPickler: BSONPickler[BSONDocument] = new BSONPickler[BSONDocument] {
    def pickle(doc: BSONDocument): BSONValue = doc
    def unpickle(v: BSONValue, path: List[String]): String \/ BSONDocument =
      typecheck[BSONDocument](v, path)(identity)
  }

  implicit def OptionBSONPickler[T](implicit bp: BSONPickler[T]): BSONPickler[Option[T]] = new BSONPickler[Option[T]] {
    def pickle(t: Option[T]): BSONValue = t.map(bp.pickle).getOrElse(BSONUndefined)
    def unpickle(v: BSONValue, path: List[String]): String \/ Option[T] =
      if (v == BSONUndefined || v == BSONNull) None.right
      else bp.unpickle(v, path).map(Some(_))
  }

  implicit def ListBSONPickler[T](implicit bp: BSONPickler[T]): BSONPickler[List[T]] = new BSONPickler[List[T]] {
    def pickle(t: List[T]): BSONValue = BSONArray(t.map(bp.pickle))
    def unpickle(v: BSONValue, path: List[String]): String \/ List[T] = for {
      ps <- typecheck[BSONArray](v, path)(_.values.map(bp.unpickle(_, path)))
      r  <- ps.toList.sequence[({type t[T] = String \/ T})#t, T]
    } yield r
  }

  implicit def HNilBSONPickler: BSONPickler[HNil] =
    new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONDocument()
      def unpickle(v: BSONValue, path: List[String]): String \/ HNil = HNil.right
    }

  implicit def HListBSONPickler[H, T <: HList]
    (implicit hbp: BSONPickler[H], tbp: BSONPickler[T]): BSONPickler[H :: T] =
    new BSONPickler[H :: T] {
      def pickle(l: H :: T): BSONValue = {
        val h = BSONArray(hbp.pickle(l.head))
        val t = tbp.pickle(l.tail)
        h ++ (if (t == BSONDocument()) BSONArray() else t).asInstanceOf[BSONArray]
      }
      def unpickle(v: BSONValue, path: List[String]): String \/ (H :: T) = ???
    }

  implicit def RecordBSONPickler[F, V, T <: HList]
    (implicit hbp: BSONPickler[V], tbp: BSONPickler[T], wk: Witness.Aux[F]): BSONPickler[FieldType[F, V] :: T] =
    new BSONPickler[FieldType[F, V] :: T] {
      val name = wk.value match {
        case s: Symbol => s.toString.drop(1)
        case a: Any    => a.toString
      }
      def pickle(l: FieldType[F, V] :: T): BSONValue = {
        val d = BSONDocument(wk.value.toString -> hbp.pickle(l.head:V))
        tbp.pickle(l.tail) match {
          case a: BSONArray => BSONArray(d) ++ a
          case x =>
            val r = d ++ x.asInstanceOf[BSONDocument]
            BSONDocument(r.elements.filter(_._2 != BSONUndefined))
        }
      }
      def unpickle(v: BSONValue, path: List[String]): String \/ (FieldType[F, V] :: T) = for {
        d <- typecheck[BSONDocument](v, path)(identity)
        v <- d.get(name).cata(_.right, s"""field `${(path :+ name).mkString(".")}' not found""".left)
        h <- hbp.unpickle(v, path :+ name)
        t <- tbp.unpickle(BSONDocument(d.elements.filter(_ != (name, v))), path)
      } yield field[F](h) :: t
    }

  implicit def BSONPicklerI: LabelledTypeClass[BSONPickler] = new LabelledTypeClass[BSONPickler] {

    def emptyProduct: BSONPickler[HNil] = new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONDocument()
      def unpickle(b: BSONValue, path: List[String]): String \/ HNil = HNil.right
    }

    override def product[H, T <: HList](name: String, BPH: BSONPickler[H], BPT: BSONPickler[T]): BSONPickler[H :: T] =
      new BSONPickler[H :: T] {
        def pickle(l: H :: T): BSONValue = {
          val r = BSONDocument(name -> BPH.pickle(l.head)) ++ BPT.pickle(l.tail).asInstanceOf[BSONDocument]
          BSONDocument(r.elements.filter(_._2 != BSONUndefined))
        }
        def unpickle(v: BSONValue, path: List[String]): String \/ (H :: T) = for {
          d <- typecheck[BSONDocument](v, path)(identity)
          v <- d.get(name).cata(_.right, BSONUndefined.right)
          h <- BPH.unpickle(v, path :+ name)
          t <- BPT.unpickle(BSONDocument(d.elements.filter(_ != (name, v))))
        } yield h :: t
      }

    def emptyCoproduct: BSONPickler[CNil] = new BSONPickler[CNil] {
      def pickle(cnil: CNil): BSONValue = BSONDocument()
      def unpickle(b: BSONValue, path: List[String]): String \/ CNil = "".left
    }

    def coproduct[L, R <: Coproduct](name: String, BPL: => BSONPickler[L], BPR: => BSONPickler[R]): BSONPickler[L :+: R] =
      new BSONPickler[L :+: R] {
        def pickle(c: L :+: R): BSONValue = c match {
          case Inl(l) => BPL.pickle(l)
          case Inr(r) => BPR.pickle(r)
        }
        def unpickle(v: BSONValue, path: List[String]): String \/ (L :+: R) =
          BPR.unpickle(v, path).map(Inr[L, R](_)).orElse(BPL.unpickle(v, path).map(r => Inl[L, R](r)))
      }

    def project[F, G](instance: => BSONPickler[G], to: F => G, from: G => F): BSONPickler[F] =
      new BSONPickler[F] {
        def pickle(f: F): BSONValue = instance.pickle(to(f))
        def unpickle(v: BSONValue, path: List[String]): String \/ F = instance.unpickle(v, path).map(from)
      }
  }
}
