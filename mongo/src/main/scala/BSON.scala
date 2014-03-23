package gkit.mongo

import reactivemongo.bson.{ BSONValue, BSONDocument, BSONArray }

import scalaz._

object BSON {

  def toBSON[A](a: A)(implicit bp: BSONPickler[A]) = bp.pickle(a)

  def fromBSON[A](v: BSONValue)(implicit bp: BSONPickler[A]) = bp.unpickle(v)

  def toBSONDoc[A](a: A)(implicit bp: BSONPickler[A]) = bp.pickle(a).asInstanceOf[BSONDocument]

  def toBSONArray[A](a: A)(implicit bp: BSONPickler[A]) = bp.pickle(a).asInstanceOf[BSONArray]

  def proj[A](implicit bp: BSONProj[A]) = bp.proj

  def pretty(v: BSONValue) = v match {
    case d: BSONDocument => BSONDocument.pretty(d)
    case x               => x.toString
  }
}
