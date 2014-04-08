package gkit.sql

import java.sql.{ SQLException, ResultSet, PreparedStatement, Date => SqlDate }

import java.util.{ Date => JavaDate }

import org.joda.time.DateTime

import scala.language.experimental.macros

import scalaz._
import scalaz.syntax.either._

import shapeless._
import shapeless.record._

trait FromResultSet[A] {

  def fromResultSet(rs: ResultSet, col: Int = 1): String \/ A

  def fromResultSet(rs: ResultSet)(f: ResultSet => A): String \/ A =
    try {
      f(rs).right
    } catch {
      case e: SQLException => e.getMessage.left
    }
}

object FromResultSet {

  implicit def apply[A](implicit ev: LabelledProductTypeClass[FromResultSet]): FromResultSet[A] =
    macro GenericMacros.deriveProductInstance[FromResultSet, A]

  implicit def BooleanFromResultSet: FromResultSet[Boolean] = new FromResultSet[Boolean] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Boolean =
      fromResultSet(rs)(_.getBoolean(col))
  }

  implicit def StringFromResultSet: FromResultSet[String] = new FromResultSet[String] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ String =
      fromResultSet(rs)(_.getString(col))
  }

  implicit def ShortFromResultSet: FromResultSet[Short] = new FromResultSet[Short] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Short =
      fromResultSet(rs)(_.getShort(col))
  }

  implicit def IntFromResultSet: FromResultSet[Int] = new FromResultSet[Int] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Int =
      fromResultSet(rs)(_.getInt(col))
  }

  implicit def LongFromResultSet: FromResultSet[Long] = new FromResultSet[Long] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Long =
      fromResultSet(rs)(_.getLong(col))
  }

  implicit def FloatFromResultSet: FromResultSet[Float] = new FromResultSet[Float] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Float =
      fromResultSet(rs)(_.getFloat(col))
  }

  implicit def DoubleFromResultSet: FromResultSet[Double] = new FromResultSet[Double] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ Double =
      fromResultSet(rs)(_.getDouble(col))
  }

  implicit def SqlDateFromResultSet: FromResultSet[SqlDate] = new FromResultSet[SqlDate] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ SqlDate =
      fromResultSet(rs)(_.getDate(col))
  }

  implicit def JavaDateFromResultSet: FromResultSet[JavaDate] = new FromResultSet[JavaDate] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ JavaDate =
      fromResultSet(rs)(_.getDate(col))
  }

  implicit def JodaDateTimeFromResultSet: FromResultSet[DateTime] = new FromResultSet[DateTime] {
    def fromResultSet(rs: ResultSet, col: Int): String \/ DateTime =
      fromResultSet(rs) { rs =>
        val r = rs.getDate(col)
        if (r != null) new DateTime(r.getTime) else null
      }
  }

  implicit def OptionFromResultSet[A](implicit frs: FromResultSet[A]): FromResultSet[Option[A]] =
    new FromResultSet[Option[A]] {
      def fromResultSet(rs: ResultSet, col: Int): String \/ Option[A] =
        frs.fromResultSet(rs, col).map(r => if (r != null) Some(r) else None)
    }

  implicit def HNilFromResultSet: FromResultSet[HNil] =
    new FromResultSet[HNil] {
      def fromResultSet(rs: ResultSet, col: Int): String \/ HNil = HNil.right
    }

  implicit def HListFromResultSet[H, T <: HList]
    (implicit hfrs: FromResultSet[H], tfrs: FromResultSet[T]): FromResultSet[H :: T] =
    new FromResultSet[H :: T] {
      def fromResultSet(rs: ResultSet, col: Int): String \/ (H :: T) =
        for {
          h <- hfrs.fromResultSet(rs, col)
          t <- tfrs.fromResultSet(rs, col+1)
        } yield h :: t
    }

  implicit def RecordFromResultSet[K, V, T <: HList]
    (implicit hfrs: FromResultSet[V], tfrs: FromResultSet[T], wk: Witness.Aux[K]): FromResultSet[FieldType[K, V] :: T] =
    new FromResultSet[FieldType[K, V] :: T] {
      def fromResultSet(rs: ResultSet, col: Int): String \/ (FieldType[K, V] :: T) =
        for {
          h <- hfrs.fromResultSet(rs, col)
          t <- tfrs.fromResultSet(rs, col+1)
        } yield field[K](h) :: t
    }

  implicit def FromResultSetI: ProductTypeClass[FromResultSet] = new ProductTypeClass[FromResultSet] {

    def emptyProduct: FromResultSet[shapeless.HNil] = new FromResultSet[shapeless.HNil] {
      def fromResultSet(rs: ResultSet, col: Int): String \/ HNil = HNil.right
    }

    def product[H, T <: HList](head: FromResultSet[H], tail: FromResultSet[T]): FromResultSet[H :: T] =
      new FromResultSet[H :: T] {
        def fromResultSet(rs: ResultSet, col: Int): String \/ (H :: T) = {
          for {
            h <- head.fromResultSet(rs, col)
            t <- tail.fromResultSet(rs, col+1)
          } yield h :: t
        }
      }

    def project[F, G](instance: => FromResultSet[G], to: F => G, from: G => F): FromResultSet[F] =
      new FromResultSet[F] {
        def fromResultSet(rs: ResultSet, col: Int): String \/ F =
          instance.fromResultSet(rs, col).map(from)
      }
  }
}
