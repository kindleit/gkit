package gkit.mongo

import reactivemongo.bson.{ BSONValue, BSONDocument }

import scalaz._
import scalaz.syntax.either._

trait QueryPicklerInstances {

  implicit def EmptyQBSONPickler = new BSONPickler[EmptyQ.type] {
    def pickle(emptyQ: EmptyQ.type) = BSONDocument()
    def unpickle(v: BSONValue, path: List[String]) = EmptyQ.right
  }
}
