package gmongo

import reactivemongo.bson.{ BSONValue, BSONDocument }

import scalaz._
import scalaz.syntax.either._

trait QueryPicklers {

  implicit def EmptyQBSONPickler = new BSONPickler[EmptyQ.type] {
    def pickle(emptyQ: EmptyQ.type) = BSONDocument()
    def unpickle(v: BSONValue) = EmptyQ.right
  }
}
