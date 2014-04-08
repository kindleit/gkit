package play.modules.gjson

import org.joda.time.DateTime

import org.specs2.mutable._

import play.api.libs.json._

class JSONPicklerSpec extends Specification {

  import JSON.{toJSON, fromJSON}

  "pseudo-inverse" in {
    val p = Prod1("John", Some(5))
    fromJSON[Prod1](toJSON(p)).toEither must beRight(p)
  }

  "fromJSON" should {
    "accepts optional values" in {
      fromJSON[Prod1](Json.obj("name" -> "John")).toEither must beRight(Prod1("John", None))
    }
    "returns error message" in {
      val errMsg1 = "type mismatch at `name': expected: play.api.libs.json.JsString, found: null"
      val errMsg2 = "type mismatch at `p1.name': expected: play.api.libs.json.JsString, found: null"
      val js1 = Json.obj("value" -> 1)
      val js2 = Json.obj("name" -> "John", "value" -> 1, "p1" -> Json.obj("value" -> 1))
      fromJSON[Prod1](js1).toEither must beLeft(errMsg1)
      fromJSON[Prod2](js2).toEither must beLeft(errMsg2)
    }
  }

  case class Prod1(name: String, value: Option[Int])
  case class Prod2(name: String, value: Option[Int], p1: Prod1)
}
