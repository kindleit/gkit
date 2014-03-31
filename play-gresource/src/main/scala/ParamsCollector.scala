package play.modules.gresource

import java.util.Date

import org.joda.time.DateTime

import play.api.mvc._

import play.core.Router._
import play.core._

import scala.language.experimental.macros

import scalaz.\/

import scalaz.syntax.std.option._
import scalaz.syntax.either._

import shapeless._

trait ParamsCollector[A] {

  def collect(params: RouteParams, name: String = ""): String \/ A

  def fromQuery[A](params: RouteParams, name: String)(implicit binder: QueryStringBindable[A]) =
    binder.bind(name, params.queryString).cata(\/.fromEither, s"Missing parameter: $name".left)
}

object ParamsCollector {

  implicit def apply[A]: ParamsCollector[A] = macro TypeClass.derive_impl[ParamsCollector, A]

  implicit def QueryStringParamsCollector[A](implicit binder: QueryStringBindable[A]): ParamsCollector[A] =
    new ParamsCollector[A] {
      def collect(params: RouteParams, name: String): String \/ A = fromQuery(params, name)(binder)
    }

  implicit def OptionParamsCollector[A](implicit pc: ParamsCollector[A]): ParamsCollector[Option[A]] =
    new ParamsCollector[Option[A]] {
      def collect(params: RouteParams, name: String): String \/ Option[A] =
        pc.collect(params, name).fold(_ => None.right, Some(_).right)
    }

  implicit def ParamsCollectorI: TypeClass[ParamsCollector] = new TypeClass[ParamsCollector] {

    def emptyProduct: ParamsCollector[HNil] = new ParamsCollector[HNil] {
      def collect(params: RouteParams, name: String): String \/ HNil = HNil.right
    }

    def product[H, T <: HList](head: ParamsCollector[H], tail: ParamsCollector[T]): ParamsCollector[H :: T] =
      new ParamsCollector[H :: T] {
        def collect(params: RouteParams, name: String): String \/ (H :: T) = ???
      }

    override def namedProduct[H, T <: HList](PCH: ParamsCollector[H], name: String, PCT: ParamsCollector[T]): ParamsCollector[H :: T] =
      new ParamsCollector[H :: T] {
        def collect(params: RouteParams, _name: String): String \/ (H :: T) = for {
          h <- PCH.collect(params, name)
          t <- PCT.collect(params, "")
        } yield h :: t
      }

    override def namedField[F](instance: ParamsCollector[F], name: String): ParamsCollector[F] =
      new ParamsCollector[F] {
        def collect(params: RouteParams, name: String): String \/ F =
          instance.collect(params, name)
      }

    def coproduct[L, R <: Coproduct](BL: => ParamsCollector[L], BR: => ParamsCollector[R]): ParamsCollector[L :+: R] =
      new ParamsCollector[L :+: R] {
        def collect(params: RouteParams, name: String): String \/ (L :+: R) = ???
      }

    def project[F, G](instance: => ParamsCollector[G], to: F => G, from: G => F): ParamsCollector[F] =
      new ParamsCollector[F] {
        def collect(params: RouteParams, name: String): String \/ F =
          instance.collect(params, name).map(from)
      }
  }
}
