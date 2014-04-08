package gkit.mongo

import org.joda.time.DateTime

import org.specs2.mutable._

import reactivemongo.bson._

class BSONPicklerSpec extends Specification {

  import BSON.{toBSON, fromBSON}

  "pseudo-inverse" in {
    val p = Prod1("John", Some(5))
    fromBSON[Prod1](toBSON(p)).toEither must beRight(p)
  }

  "fromBSON" should {
    "accepts optional values" in {
      fromBSON[Prod1](BSONDocument("name" -> "John")).toEither must beRight(Prod1("John", None))
    }
    "returns error message" in {
      val errMsg1 = "type mismatch at `name': expected: reactivemongo.bson.BSONString, found: BSONUndefined"
      val errMsg2 = "type mismatch at `p1.name': expected: reactivemongo.bson.BSONString, found: BSONUndefined"
      val js1 = BSONDocument("value" -> 1)
      val js2 = BSONDocument("name" -> "John", "value" -> 1, "p1" -> BSONDocument("value" -> 1))
      fromBSON[Prod1](js1).toEither must beLeft(errMsg1)
      fromBSON[Prod2](js2).toEither must beLeft(errMsg2)
    }
  }

  case class Prod1(name: String, value: Option[Int])
  case class Prod2(name: String, value: Option[Int], p1: Prod1)
}
