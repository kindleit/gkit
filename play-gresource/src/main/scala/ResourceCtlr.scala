package play.modules.gresource

import play.api._
import play.api.mvc._

trait ResourceCtlr[ID] extends Controller {

  import ResourceCtlr._

  def perms: Map[ResourceOp, Perm]

  def projs: Map[ResourceOp, List[Field]]

  def find: EssentialAction

  def insert: EssentialAction

  def get(id: ID): EssentialAction

  def update(id: ID): EssentialAction

  def delete(id: ID): EssentialAction
}

object ResourceCtlr {

  type Perm = Function1[RequestHeader, Boolean]
  type Field = String

  val Anonymous = (_: RequestHeader) => true

  sealed trait ResourceOp
  case object Find extends ResourceOp
  case object Insert extends ResourceOp
  case object Get extends ResourceOp
  case object Update extends ResourceOp
  case object Delete extends ResourceOp
}
