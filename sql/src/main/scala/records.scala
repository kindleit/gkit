package gkit.sql

import shapeless._
import shapeless.record._
import shapeless.ops.record._

final class RecordOps[L <: HList](l: L) {

  def getf(k: Witness)(implicit selector : FieldSelector[L, k.T]): selector.Out = selector(l)

  def get(k: Witness)(implicit selector : Selector[L, k.T]): selector.Out = selector(l)

  def -[V, Out <: HList](k: Witness)(implicit remover : Remover.Aux[L, k.T, (V, Out)]): Out = remover(l)._2
}

@annotation.implicitNotFound(msg = "No field ${K} in record ${L}")
trait FieldSelector[L <: HList, K] {
  type Out
  def apply(l : L): Out
}

trait LowPriorityFieldSelector {
  type Aux[L <: HList, K, Out0] = FieldSelector[L, K] { type Out = Out0 }

  implicit def hlistSelect[H, T <: HList, K]
    (implicit st : FieldSelector[T, K]): Aux[H :: T, K, st.Out] =
    new FieldSelector[H :: T, K] {
      type Out = st.Out
      def apply(l : H :: T): Out = st(l.tail)
    }
}

object FieldSelector extends LowPriorityFieldSelector {
  def apply[L <: HList, K](implicit selector: FieldSelector[L, K]): Aux[L, K, selector.Out] = selector

  implicit def hlistSelect1[K, V, T <: HList]: Aux[FieldType[K, V] :: T, K, FieldType[K, V]] =
    new FieldSelector[FieldType[K, V] :: T, K] {
      type Out = FieldType[K, V]
      def apply(l : FieldType[K, V] :: T): Out = l.head
    }
}
