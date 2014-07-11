# GKit #

GKit is a set of combinator libraries which can ultimately be turned into powerful, composable REST servers.

### Download ###

The gkit packages should make there way into oss.sonatype.com and eventually maven central. But you can always download them manually, or be notified of version updates on bintray:

[ ![Download](https://api.bintray.com/packages/kitsd/releases/gkit/images/download.png) ](https://bintray.com/kitsd/releases/gkit/_latestVersion)

### It's written on top of [shapeless](https://github.com/milessabin/shapeless) & [scalaz](https://github.com/scalaz/scalaz/)  ###


The point of GKit is to eliminate as much boilerplate as possible while at the same time allowing for powerful extension and composition points in the handling of stateless http requests.

You can quickly write a RESTful resource with a find query with the following code:

```
#!scala

import reactivemongo.bson.BSONObjectID

case class Todo
  (
    _id             : BSONObjectID
  , description     : String
  , completed       : Boolean
  )

object Todos extends TodoFunctions {
  lazy val httpApi = Http.get(Prefix)(httpFind)
}

trait TodoFunctions extends QueryFunctions {

  val cname = "todos"

  val idLens = lens[Todo] >> '_id

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
