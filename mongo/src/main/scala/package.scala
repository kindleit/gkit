package gkit

import reactivemongo.bson.BSONValue

package object mongo extends QueryPicklerInstances with GeneratorInstances {
  type BSONPickler[A] = Pickler[A, BSONValue]
}
