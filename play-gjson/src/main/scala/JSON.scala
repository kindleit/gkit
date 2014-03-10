package play.modules.gjson

import play.api.libs.json.JsValue

object JSON {

  def toJSON[A](a: A)(implicit jp: JSONPickler[A]): JsValue = jp.pickle(a)

  def fromJSON[A](v: JsValue)(implicit jp: JSONPickler[A]) = jp.unpickle(v)
}
