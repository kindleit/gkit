package gkit.mongo

import reactivemongo.api._
import reactivemongo.core.nodeset.Authenticate

import scala.concurrent.ExecutionContext

case class DbEnv(dbName: String, servers: List[String] = List("localhost"), auth: List[Authenticate], cpn: Option[Int])
  (implicit ec: ExecutionContext) {
  lazy val driver = new MongoDriver
  lazy val connection = driver.connection(servers)
  lazy val db = connection(dbName)

  def collection(cname: String) = new Collection(db, cname)
}
