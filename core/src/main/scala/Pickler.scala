package gkit

import scalaz._
import scalaz.syntax.std.option._
import scalaz.syntax.either._

import shapeless._
import shapeless.syntax.typeable._

trait Pickler[A, B] {

  def pickle(a: A): B

  def unpickle(b: B, path: List[String] = Nil): String \/ A

  class Typecheck[A] {
    def apply[B, C](b: B, path: List[String])(f: A => C)
      (implicit tpble: Typeable[A], tm: Manifest[A]): String \/ C = {
      def errMsg = s"""type mismatch at `${path.mkString(".")}': expected: ${tm.runtimeClass.getName}, found: ${b.toString}"""
      b.cast[A].cata(f(_).right, errMsg.left)
    }
  }

  def typecheck[A]: Typecheck[A] = new Typecheck[A]
}
