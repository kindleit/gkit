package play.modules.gresource.mongo

import gkit.mongo.DbEnv

import play.api._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import reactivemongo.api._
import reactivemongo.core.commands._
import reactivemongo.core.nodeset.Authenticate

import scala.collection.JavaConversions._

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext }
import scala.util.{ Failure, Success }

import scalaz._
import Scalaz._

class GMongoPlugin(app: Application) extends Plugin {
  private var _dbEnv: Option[DbEnv] = None

  def dbEnv = _dbEnv.getOrElse(throw new RuntimeException("GMongoPlugin: no DbEnv available"))

  override def onStart = {
    _dbEnv = {
      val conf = GMongoPlugin.conf(app)
      try Some(DbEnv(conf._1, conf._2, conf._3, conf._4))
      catch {
        case e: Throwable =>
          throw new PlayException("GMongoPlugin", "An exception occurred during initializing.", e)
      }
    }
  }

  override def onStop = {
    _dbEnv.map(h => Await.ready(h.connection.askClose()(10.seconds), 10.seconds))
    _dbEnv = None
  }
}

object GMongoPlugin {

  def dbEnv(implicit app: Application) = current.dbEnv

  def current(implicit app: Application): GMongoPlugin =
    app.plugin[GMongoPlugin].getOrElse(throw new PlayException("GMongoPlugin", "The GMongoPlugin has not been initialized!"))

  private def conf(app: Application): (String, List[String], List[Authenticate], Option[Int]) = {
    val c = app.configuration
    val (dbName, servers, auth) = c.getString("mongodb.uri") match {
      case Some(uri) =>
        MongoConnection.parseURI(uri) match {
          case Success(MongoConnection.ParsedURI(hosts, _, _, Some(db), auth)) =>
            (db, hosts.map(h => h._1 + ":" + h._2), auth.toList)
          case Success(MongoConnection.ParsedURI(_, _, _, None, _)) =>
            throw c.globalError(s"Missing database name in mongodb.uri '$uri'")
          case Failure(e) => throw c.globalError(s"Invalid mongodb.uri '$uri'", Some(e))
        }
      case _ =>
        (c.getString("mongodb.db").getOrElse(throw c.globalError("Missing configuration key 'mongodb.db'!")),
          c.getStringList("mongodb.servers").cata(_.toList, List("localhost:27017")),
          List())
    }
    (dbName, servers, auth, c.getInt("mongodb.channels"))
  }
}
