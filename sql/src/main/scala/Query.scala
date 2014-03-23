package gkit.sql

import java.sql._

import scala.collection.mutable.ListBuffer

import scalaz.effect._

import scalaz.std.effect.AllEffectInstances._
import scalaz.std.list._
import scalaz.std.option._

import scalaz.syntax.std.boolean._
import scalaz.syntax.apply._
import scalaz.syntax.effect.all._
import scalaz.syntax.either._
import scalaz.syntax.plus._

import scalaz._

import shapeless._
import shapeless.ops.hlist._

case class Query[A <: HList, B <: HList, C <: HList]
  (
    table       : Table[A]
  , project     : A => B
  , restriction : Option[A => Exp[_]]
  , relations   : List[String]
  , offset      : Option[Int]
  , limit       : Option[Int]
  )
  (implicit
    val row: Row.Aux[B, C]
  ) {

  def map[D <: HList, E <: HList](f: A => D)(implicit row: Row.Aux[D, E]) =
    copy(project = f)

  def flatMap[D <: HList, E <: HList, F <: HList](f: A => Query[D, E, F])
    (implicit isc: IsHCons[D], row: Row.Aux[E, F]) = {
    val q = f(*)
    val e1: Option[D => Exp[_]] = (restriction |@| q.restriction)((g, h) => (d: D) => g(*) && h(d))
    val e2 = restriction.map(g => (d: D) => g(*))
    val e3 = q.restriction
    q.copy(restriction = e1 <+> e2 <+> e3, relations = table.name :: relations)
  }

  def filter(f: A => Exp[_]) = copy(restriction = Some(f))

  def drop(n: Int) = copy(offset = Some(n))

  def take(n: Int) = copy(limit = Some(n))

  def insert(a: C)(implicit ts: ToStatement[C]): Kleisli[IO, Connection, String \/ Int] =
    execUpdate(insertStatement, a)

  def update(a: C)(implicit ts: ToStatement[C]): Kleisli[IO, Connection, String \/ Int] =
    execUpdate(updateStatement, a)

  def delete: Kleisli[IO, Connection, Int] = for {
    s <- Kleisli.ask[IO, Connection].map(_.createStatement)
    r <- IO(s.executeUpdate(deleteStatement)).liftIO[KIOC]
    _ <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield r

  def first(implicit frs: FromResultSet[C]): Kleisli[IO, Connection, String \/ Option[C]] = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(selectStatement))
    rs <- IO(s.executeQuery()).liftIO[KIOC]
    a  <- IO(rs.next().fold(frs.fromResultSet(rs, 1).map(Some(_)), None.right)).liftIO[KIOC]
    _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield a

  def toList(implicit frs: FromResultSet[C]): Kleisli[IO, Connection, String \/ List[C]] = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(selectStatement))
    rs <- IO(s.executeQuery()).liftIO[KIOC]
    as <- readAll(rs).liftIO[KIOC]
    _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield as

  def * : A = table.*

  private def mkSS98pagQry(s: Select, offset: Int, limit: Int) = {
    val pkName = s.cols.head.label
    With("results",
      s.copy(cols = s.cols :+ Column[Unit](s"ROW_NUMBER() over (order by ${pkName})", "rowNum")),
      Select(List(Column[Unit]("*")),
        From(List("results")) ::
          Where(And(GTE(Column[Int]("rowNum"), Literal(offset)), LTE(Column[Int]("rowNum"), Literal(offset + limit)))) :: Nil))
  }

  private lazy val evalRestriction =
    restriction.map(f => Where(f(*)))

  lazy val selectStatement = {
    val s = Select(row.columns(project(*)), From(table.name :: relations) :: evalRestriction.toList)
    val t = (offset |@| limit)((m, n) => mkSS98pagQry(s, m, n)).getOrElse(s)
    showAST(t)
  }

  lazy val insertStatement =
    showAST(Insert(table.name, row.columns(project(*))))

  lazy val updateStatement =
    showAST(Update(table.name, row.columns(project(*)), evalRestriction))

  lazy val deleteStatement =
    showAST(Delete(From(List(table.name)), evalRestriction))

  type KIOC[A] = Kleisli[IO, Connection, A]

  private def readAll[A](rs: ResultSet)(implicit frs: FromResultSet[A]) = IO {
    import scalaz.syntax.traverse._
    val lf = new ListBuffer[String \/ A]
    while (rs.next()) lf += frs.fromResultSet(rs, 1)
    lf.toList.sequence[({type λ[A] = String \/ A})#λ, A]
  }

  private def execUpdate[A](q: String, a: A)(implicit ts: ToStatement[A]) = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(q))
    fr <- IO(ts.toStatement(a, s)).liftIO[KIOC]
    er <- fr.traverse(s => IO(s.executeUpdate()).liftIO[KIOC])
    _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield er
}
