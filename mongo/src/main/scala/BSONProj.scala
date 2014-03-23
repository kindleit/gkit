package gkit.mongo

import org.joda.time.DateTime

import reactivemongo.bson._

import scala.language.experimental.macros

import shapeless._

trait BSONProj[T] {
  def proj: BSONDocument
}

object BSONProj {

  implicit def apply[T]: BSONProj[T] = macro TypeClass.derive_impl[BSONProj, T]

  def default[T] = new BSONProj[T] { def proj = BSONDocument() }

  implicit def StringBSONProj = default[String]
  implicit def BooleanBSONProj = default[Boolean]
  implicit def IntBSONProj = default[Int]
  implicit def DoubleBSONProj = default[Double]
  implicit def DateTimeBSONProj = default[DateTime]
  implicit def BSONObjectIDBSONProj = default[BSONObjectID]
  implicit def ListBSONProj[T] = default[List[T]]

  implicit def OptionBSONProj[T](implicit bp: BSONProj[T]) = new BSONProj[Option[T]] {
    def proj = bp.proj
  }

  implicit def BSONProjI: ProductTypeClass[BSONProj] = new ProductTypeClass[BSONProj] {

    def emptyProduct = new BSONProj[HNil] {
      def proj = BSONDocument()
    }

    def product[H, T <: HList](head: BSONProj[H], tail: BSONProj[T]) = ???

    override def namedProduct[H, T <: HList](head: BSONProj[H], name: String, tail: BSONProj[T]) =
      new BSONProj[H :: T] {
        def proj = {
          val p = head.proj
          val h =
            if (p.isEmpty) BSONDocument(name -> BSONInteger(1))
            else BSONDocument(name -> p)
          h ++ tail.proj
        }
      }

    override def namedField[F](instance: BSONProj[F], name: String) =
      new BSONProj[F] {
        def proj = BSONDocument(name -> BSONInteger(1))
      }

    def project[F, G](instance: => BSONProj[G], to: F => G, from: G => F) =
      new BSONProj[F] {
        def proj = instance.proj
      }
  }
}
