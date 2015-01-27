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
      typecheck[BSONDouble](v, path)(_.value) |||
      typecheck[BSONInteger](v, path)(_.value).map(_.toDouble)
  }

  implicit def IntBSONPickler: BSONPickler[Int] = new BSONPickler[Int] {
    def pickle(s: Int): BSONValue = BSONInteger(s)
    def unpickle(v: BSONValue, path: List[String]): String \/ Int =
      typecheck[BSONInteger](v, path)(_.value) |||
      typecheck[BSONDouble](v, path)(_.value).map(_.toInt)
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
    def pickle(t: Option[T]): BSONValue = t.map(bp.pickle) | BSONUndefined
    def unpickle(v: BSONValue, path: List[String]): String \/ Option[T] =
      if (v == BSONUndefined || v == BSONNull) None.right
      else bp.unpickle(v, path).map(Some(_))
  }

  implicit def ListBSONPickler[T](implicit bp: BSONPickler[T]): BSONPickler[List[T]] = new BSONPickler[List[T]] {
    def pickle(t: List[T]): BSONValue = BSONArray(t.map(bp.pickle))
    def unpickle(v: BSONValue, path: List[String]): String \/ List[T] = for {
      ps <- typecheck[BSONArray](v, path)(_.values.map(bp.unpickle(_, path)))
      r  <- ps.toList.sequenceU
    } yield r
  }

  implicit def HNilBSONPickler: BSONPickler[HNil] =
    new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONArray()
      def unpickle(v: BSONValue, path: List[String]): String \/ HNil = HNil.right
    }

  implicit def HListBSONPickler[H, T <: HList]
    (implicit hbp: BSONPickler[H], tbp: BSONPickler[T]): BSONPickler[H :: T] =
    new BSONPickler[H :: T] {
      def pickle(l: H :: T): BSONValue = BSONArray(hbp.pickle(l.head)) ++ tbp.pickle(l.tail).asInstanceOf[BSONArray]
      def unpickle(v: BSONValue, path: List[String]): String \/ (H :: T) = v match {
        case a: BSONArray => for { h <- hbp.unpickle (a.values.head); t <- tbp.unpickle (BSONArray(a.values.tail)) } yield h :: t
        case _ => "Only BSONArrays can be unpickled to an HList".left
      }
    }

  implicit def RecordBSONPickler[F, V, T <: HList]
    (implicit hbp: BSONPickler[V], tbp: BSONPickler[T], wk: Witness.Aux[F]): BSONPickler[FieldType[F, V] :: T] =
    new BSONPickler[FieldType[F, V] :: T] {
      import scala.util.Try
      val name = wk.value match {
        case s: Symbol => s.toString.drop(1)
        case a: Any    => a.toString
      }
      def pickle(l: FieldType[F, V] :: T): BSONValue = hbp.pickle(l.head:V) match {
        case BSONUndefined if l.tail == HNil => BSONDocument()
        case BSONUndefined => tbp.pickle(l.tail)
        case v if l.tail == HNil => BSONDocument(name -> v)
        case v => BSONDocument(Try(name -> v) #:: (tbp.pickle(l.tail).asInstanceOf[BSONDocument]).stream)
      }
      def unpickle(v: BSONValue, path: List[String]): String \/ (FieldType[F, V] :: T) = for {
        d <- typecheck[BSONDocument](v, path)(identity)
        v <- d.get(name).cata(_.right, s"""field `${(path :+ name).mkString(".")}' not found""".left)
        o =  d.elements.withFilter(_._1 != name).map(identity)
        h <- hbp.unpickle(v, path :+ name)
        t <- tbp.unpickle(BSONDocument(o), path)
      } yield field[F](h) :: t
    }

  implicit def BSONPicklerI: LabelledTypeClass[BSONPickler] = new LabelledTypeClass[BSONPickler] {

    def emptyProduct: BSONPickler[HNil] = new BSONPickler[HNil] {
      def pickle(nil: HNil): BSONValue = BSONDocument()
      def unpickle(b: BSONValue, path: List[String]): String \/ HNil = HNil.right
    }

    def product[H, T <: HList](name: String, BPH: BSONPickler[H], BPT: BSONPickler[T]): BSONPickler[H :: T] =
      new BSONPickler[H :: T] {
        import scala.util.Try
        def pickle(l: H :: T): BSONValue = BPH.pickle(l.head) match {
          case BSONUndefined if l.tail == HNil => BSONDocument()
          case BSONUndefined => BPT.pickle(l.tail)
          case v if l.tail == HNil => BSONDocument(name -> v)
          case v => BSONDocument(Try(name -> v) #:: (BPT.pickle(l.tail).asInstanceOf[BSONDocument]).stream)
        }
        def unpickle(v: BSONValue, path: List[String]): String \/ (H :: T) = for {
          d <- typecheck[BSONDocument](v, path)(identity)
          v <- d.get(name).cata(_.right, BSONUndefined.right)
          o =  d.elements.withFilter(_._1 != name).map(identity)
          h <- BPH.unpickle(v, path :+ name)
          t <- BPT.unpickle(BSONDocument(o))
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
