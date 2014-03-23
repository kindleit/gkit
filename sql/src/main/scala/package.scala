package gkit

import scala.language.experimental.macros

package object sql {

  import shapeless._

  implicit def ToColumnOps[A](a: Column[A]) = new ColumnOps(a)

  implicit def ToExpOps[A](e: Exp[A]) = new ExpOps(e)

  implicit def ToRecordOps[L <: HList](l: L) = new RecordOps(l)
}
