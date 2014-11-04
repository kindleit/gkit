package gkit

import reactivemongo.bson.{BSONValue, BSONDocument, BSONArray}

package object mongo extends QueryPicklerInstances with GeneratorInstances {
  type BSONPickler[A] = Pickler[A, BSONValue]
  type BSONDocPickler[A] = Pickler[A, BSONDocument]
  type BSONArrPickler[A] = Pickler[A, BSONArray]
}
