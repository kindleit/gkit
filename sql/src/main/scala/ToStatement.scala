package gkit.sql

import java.sql.{ Types, SQLException, PreparedStatement, Date => SqlDate }

import java.util.{ Date => JavaDate }

import org.joda.time.DateTime

import scala.language.experimental.macros

import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import shapeless._
import shapeless.record._

trait ToStatement[A] {

  def toStatement(a: A, ps: PreparedStatement, index: Int = 1): String \/ PreparedStatement

  def using[A](ps: PreparedStatement)(f: PreparedStatement => Unit): String \/ PreparedStatement =
    try { f(ps); ps.right } catch { case e: SQLException => e.getMessage.left }
}

object ToStatement {

  implicit def apply[A]: ToStatement[A] = macro GenericMacros.deriveInstance[ToStatement, A]

  implicit def BooleanToStatement: ToStatement[Boolean] = new ToStatement[Boolean] {
    def toStatement(b: Boolean, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setBoolean(index, b))
  }

  implicit def StringToStatement: ToStatement[String] = new ToStatement[String] {
    def toStatement(s: String, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setString(index, s))
  }

  implicit def ShortToStatement: ToStatement[Short] = new ToStatement[Short] {
    def toStatement(s: Short, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setShort(index, s))
  }

  implicit def IntToStatement: ToStatement[Int] = new ToStatement[Int] {
    def toStatement(i: Int, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setInt(index, i))
  }

  implicit def LongToStatement: ToStatement[Long] = new ToStatement[Long] {
    def toStatement(l: Long, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setLong(index, l))
  }

  implicit def FloatToStatement: ToStatement[Float] = new ToStatement[Float] {
    def toStatement(b: Float, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setFloat(index, b))
  }

  implicit def DoubleToStatement: ToStatement[Double] = new ToStatement[Double] {
    def toStatement(b: Double, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setDouble(index, b))
  }

  implicit def SqlDateToStatement: ToStatement[SqlDate] = new ToStatement[SqlDate] {
    def toStatement(sd: SqlDate, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setDate(index, sd))
  }

  implicit def JavaDateToStatement: ToStatement[JavaDate] = new ToStatement[JavaDate] {
    def toStatement(jd: JavaDate, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setDate(index, new SqlDate(jd.getTime)))
  }

  implicit def JodaDateTimeToStatement: ToStatement[DateTime] = new ToStatement[DateTime] {
    def toStatement(jd: DateTime, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
      using(ps)(_.setDate(index, new SqlDate(jd.getMillis)))
  }

  implicit def OptionToStatement[A](implicit ts: ToStatement[A]): ToStatement[Option[A]] =
    new ToStatement[Option[A]] {
      def toStatement(ov: Option[A], ps: PreparedStatement, index: Int): String \/ PreparedStatement =
        ov.cata(ts.toStatement(_, ps, index), ts.using(ps)(_.setNull(index, Types.NULL)))
    }

  implicit def HNilToStatement: ToStatement[HNil] =
    new ToStatement[HNil] {
      def toStatement(nil: HNil, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
        ps.right
    }

  implicit def HListToStatement[H, T <: HList]
    (implicit hts: ToStatement[H], tts: ToStatement[T]): ToStatement[H :: T] =
    new ToStatement[H :: T] {
      def toStatement(l: H :: T, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
        for {
          ps1 <- hts.toStatement(l.head, ps, index)
          ps2 <- tts.toStatement(l.tail, ps1, index+1)
        } yield ps2
    }

  implicit def RecordToStatement[K, V, T <: HList]
    (implicit hts: ToStatement[V], tts: ToStatement[T], wk: Witness.Aux[K]): ToStatement[FieldType[K, V] :: T] =
    new ToStatement[FieldType[K, V] :: T] {
      def toStatement(l: FieldType[K, V] :: T, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
        for {
          ps1 <- hts.toStatement(l.head, ps, index)
          ps2 <- tts.toStatement(l.tail, ps1, index+1)
        } yield ps2
    }

  implicit def ToStatementI: ProductTypeClass[ToStatement] = new ProductTypeClass[ToStatement] {

    def emptyProduct: ToStatement[HNil] = new ToStatement[HNil] {
      def toStatement(a: HNil, ps: PreparedStatement, index: Int): String \/ PreparedStatement = ps.right
    }

    def product[H, T <: HList](head: ToStatement[H], tail: ToStatement[T]): ToStatement[H :: T] =
      new ToStatement[H :: T] {
        def toStatement(l: H :: T, ps: PreparedStatement, index: Int): String \/ PreparedStatement = {
          head.toStatement(l.head, ps, index)
          tail.toStatement(l.tail, ps, index+1)
          ps.right
        }
      }

    def project[A, B](instance: => ToStatement[B], to: A => B, from: B => A): ToStatement[A] =
      new ToStatement[A] {
        def toStatement(a: A, ps: PreparedStatement, index: Int): String \/ PreparedStatement =
          instance.toStatement(to(a), ps, index)
      }
  }
}
