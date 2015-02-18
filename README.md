# GKit #

[![Join the chat at https://gitter.im/kindleit/gkit](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/kindleit/gkit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

GKit is a set of combinator libraries which can ultimately be turned into powerful, composable REST servers.

### Download ###

The gkit packages should make there way into oss.sonatype.com and eventually maven central. But you can always download them manually, or be notified of version updates on bintray:

[ ![Download](https://api.bintray.com/packages/kitsd/releases/gkit/images/download.png) ](https://bintray.com/kitsd/releases/gkit/_latestVersion)

### It's written on top of [shapeless](https://github.com/milessabin/shapeless) & [scalaz](https://github.com/scalaz/scalaz/)  ###


The point of GKit is to eliminate as much boilerplate as possible while at the same time allowing for powerful extension and composition points in the handling of stateless http requests.

You can quickly write a RESTful resource with a find query with the following code:

```scala

import gkit.mongo._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.Result
import play.modules.gjson._
import play.modules.gresource.Http
import play.modules.gresource.Http._
import play.modules.gresource.mongo._
import reactivemongo.bson.{BSONObjectID, BSONDocument}
import scala.concurrent.Future
import scalaz.Scalaz._
import scalaz._

case class Todo
  (
    _id             : BSONObjectID
  , description     : String
  , completed       : Boolean
  )

case class FindParams
  (
    sortby          : Option[String]
  , asc             : Option[Boolean]
  )

object Todos extends TodoFunctions {
  lazy val httpApi = Http.get(Prefix)(httpFind)
}

trait TodoFunctions extends QueryFunctions {

  val cname = "todos"

  def httpFind: Kleisli[EFE, AReq, Result] =
    paramsFromReq[FindParams].apply >>>
    liftK(mkFindQry _ >>> find[Todo](cname)) >>>
    toJsonResult

  def mkFindQry(fp: FindParams) =
    FindQuery(
      query    = EmptyQ,
      proj     = EmptyQ,
      defaults = fp)
}

trait QueryFunctions extends JSONPicklerInstances {

  case class FindQuery[A, B](query: A, proj: B, defaults: FindParams)

  class Find[A] {
    def apply[B, C](cname: String)(fq: FindQuery[B, C])
      (implicit BP1: BSONPickler[A], BP2: BSONPickler[B], BP3: BSONPickler[C]): Future[Error \/ List[A]] = {
      val sortby = fq.defaults.sortby | "_id"
      val asc = fq.defaults.asc | true
      val sdoc = BSONDocument(sortby -> asc.fold(1, -1))
      val qb = dbe.collection(cname).find(fq.query, fq.proj).sort(sdoc)
      qb.cursor[A].collect[List]().map(_.leftMap(E500(_):Error))
    }
  }

  def find[A] = new Find[A]

  def dbe = GMongoPlugin.dbEnv
}

trait JSONPicklerInstances {

  implicit def BSONObjectIDPickler: JSONPickler[BSONObjectID] =
    new JSONPickler[BSONObjectID] {
      def pickle(boid: BSONObjectID): JsValue = JsString(boid.stringify)

      def unpickle(v: JsValue, path: List[String]): String \/ BSONObjectID = {
        def parse(s: String) =
          try { BSONObjectID(s).right } catch { case e: Throwable => e.getMessage.left }

        for {
          js <- typecheck[JsString](v, path)(identity)
          id <- parse(js.value).leftMap(e => s"""error at: `${path.mkString(".")}`: $e""")
        } yield id
      }
    }
}
```


### Activator Plugin ###

We've also built an [activator plugin](https://github.com/kindleit/todo-gkit-angularjs/) which has a clear example of a RESTful service with Play!Framework and MongoDB

### Work in progress ###

These libraries are still very much a work in progress and some of the API is not finalized

We definitely need some help, and here are our planned features
* Some libraries are missing tests
* Finishing the SQL combinators
* Adding support for the new Akka http

### Contributing ###

* This library was written by Carlos Encarnación
* Feel free to drop us a line at info@kitsd.com
