package gkit.mongo

import reactivemongo.bson.{ BSONValue, BSONDocument, BSONArray }

import scalaz._

object BSON {

  def toBSON[A](a: A)(implicit bp: BSONPickler[A]) = bp.pickle(a)

  def fromBSON[A](v: BSONValue)(implicit bp: BSONPickler[A]) = bp.unpickle(v)

  def toBSONDoc[A](a: A)(implicit bp: BSONDocPickler[A]) = bp.pickle(a)

  def toBSONArray[A](a: A)(implicit bp: BSONArrPickler[A]) = bp.pickle(a)

  def proj[A](implicit bp: BSONProj[A]) = bp.proj

  def pretty(v: BSONValue): String = v match {
    case d: BSONDocument => d.elements.map(el => s"${el._1}: ${pretty(el._2)}").mkString("BSONDocument(", ", ", ")")
    case x               => x.toString
  }
}
