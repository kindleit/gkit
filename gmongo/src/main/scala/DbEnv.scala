package gmongo

import reactivemongo.api._

import scala.concurrent.ExecutionContext

case class DbEnv(dbName: String, servers: List[String] = List("localhost"))
  (implicit ec: ExecutionContext) {
  val driver = new MongoDriver
  val executionContext = ec
  val connection = driver.connection(servers)
  val db = connection(dbName)
}
