package gkit

import shapeless._

import scala.language.experimental.macros

import scala.reflect.macros.Context

trait Generator[A] {
  def generate: A
}

object Generator {

  implicit def apply[A]: Generator[A] = macro GenericMacros.deriveInstance[Generator, A]

  implicit def GeneratorI: ProductTypeClass[Generator] = new ProductTypeClass[Generator] {

    def emptyProduct: Generator[HNil] =
      new Generator[HNil] {
        def generate = HNil
      }

    def product[H, T <: HList](GH: Generator[H], GT: Generator[T]): Generator[H :: T] =
      new Generator[H :: T] {
        def generate: H :: T = GH.generate :: GT.generate
      }

    def project[F, G](instance: => Generator[G], to: F => G, from: G => F): Generator[F] =
      new Generator[F] {
        def generate: F = from(instance.generate)
      }
  }
}
