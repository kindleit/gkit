package gkit

import shapeless._

import scala.language.experimental.macros

trait Generator[A] {
  def generate: A
}

object Generator {

  implicit def apply[A]: Generator[A] = macro TypeClass.derive_impl[Generator, A]

  implicit def GeneratorI: TypeClass[Generator] = new TypeClass[Generator] {

    def emptyProduct: Generator[HNil] =
      new Generator[HNil] {
        def generate = HNil
      }

    def product[H, T <: HList](GH: Generator[H], GT: Generator[T]): Generator[H :: T] =
      new Generator[H :: T] {
        def generate: H :: T = GH.generate :: GT.generate
      }

    def coproduct[L, R <: Coproduct](GL: => Generator[L], GR: => Generator[R]): Generator[L :+: R] =
      new Generator[L :+: R] {
        def generate: L :+: R = ???
      }

    def project[F, G](instance: => Generator[G], to: F => G, from: G => F): Generator[F] =
      new Generator[F] {
        def generate: F = from(instance.generate)
      }
  }
}
