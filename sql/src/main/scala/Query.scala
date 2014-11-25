package gkit.sql

import java.sql._

import scala.collection.mutable.ListBuffer

import scalaz._
import scalaz.effect._

import scalaz.std.effect.AllEffectInstances._
import scalaz.std.list._
import scalaz.std.option._

import scalaz.syntax.effect.all._
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._

object Query {

  def resultLeft[A](s: String): Result[A] =
    EitherT(IO(s.left[A]).liftIO[KIOC])

  def resultRight[A](a: A): Result[A] =
    EitherT(IO(a.right[String]).liftIO[KIOC])

  def execute[A](q: String)(a: A)(implicit ts: ToStatement[A]): Execute =
    new Execute(for {
      s  <- EitherT[KIOC, String, PreparedStatement](Kleisli.ask[IO, Connection].map(prepareStmt(q)))
      _  <- EitherT(IO(ts.toStatement(a, s)).liftIO[KIOC])
      rs <- EitherT(IO(executeQuery(s)).liftIO[KIOC])
    } yield new ExecuteResult(s, rs))

  def execute1(q: String): Execute =
    new Execute(for {
      s  <- EitherT[KIOC, String, PreparedStatement](Kleisli.ask[IO, Connection].map(prepareStmt(q)))
      rs <- EitherT(IO(executeQuery(s)).liftIO[KIOC])
    } yield new ExecuteResult(s, rs))

  def update[A](q: String)(a: A)(implicit ts: ToStatement[A]): Update =
    new Update(for {
      ps <- EitherT[KIOC, String, PreparedStatement](Kleisli.ask[IO, Connection].map(prepareStmt(q)))
      _  <- EitherT(IO(ts.toStatement(a, ps)).liftIO[KIOC])
      er <- EitherT(IO(executeUpdate(ps)).liftIO[KIOC])
    } yield new UpdateResult(ps, er))

  def update1(q: String): Update1 =
    new Update1(for {
      s <- EitherT[KIOC, String, Statement](Kleisli.ask[IO, Connection].map(createStmt))
      r  <- EitherT(IO(executeUpdate1(s)(q)).liftIO[KIOC])
      _  <- IO(s).using(_ => IO.ioUnit).liftIO[Result]
    } yield new UpdateResult1(r))

  private def executeQuery(ps: PreparedStatement): String \/ ResultSet =
    \/.fromTryCatchNonFatal(ps.executeQuery()).leftMap(_.getMessage)

  private def executeUpdate(ps: PreparedStatement): String \/ Int =
    \/.fromTryCatchNonFatal(ps.executeUpdate()).leftMap(_.getMessage)

  private def executeUpdate1(s: Statement)(q: String): String \/ Int =
    \/.fromTryCatchNonFatal(s.executeUpdate(q)).leftMap(_.getMessage)

  private def createStmt(c: Connection): String \/ Statement =
    \/.fromTryCatchNonFatal(c.createStatement).leftMap(_.getMessage) 

  private def prepareStmt(q: String)(c: Connection): String \/ PreparedStatement =
    \/.fromTryCatchNonFatal(c.prepareStatement(q, Statement.RETURN_GENERATED_KEYS)).leftMap(_.getMessage)
}

class Execute(er: Result[ExecuteResult]) {

  def first[A](implicit frs: FromResultSet[A]): Result[Option[A]] =
    er.flatMap(_.first[A])

  def toList[A](implicit frs: FromResultSet[A]): Result[List[A]] =
    er.flatMap(_.toList[A])

  def toInt: Result[Int] =
    er.flatMap(_.first[Int]).map(_.getOrElse(0))
}

class ExecuteResult(s: PreparedStatement, rs: ResultSet) {

  def first[A](implicit frs: FromResultSet[A]): Result[Option[A]] =
    for {
      a  <- EitherT(IO(readOne(rs)).liftIO[KIOC])
      _  <- IO(s).using(_ => IO.ioUnit).liftIO[Result]
    } yield a

  def toList[A](implicit frs: FromResultSet[A]): Result[List[A]] =
    for {
      as <- EitherT(IO(readAll(rs)).liftIO[KIOC])
      _  <- IO(s).using(_ => IO.ioUnit).liftIO[Result]
    } yield as

  private def readOne[A](rs: ResultSet)(implicit frs: FromResultSet[A]): String \/ Option[A] =
    \/.fromTryCatchNonFatal(rs.next()).fold(
      _.getMessage.left,
      _.fold(frs.fromResultSet(rs, 1).map(Some(_)), None.right))

  private def readAll[A](rs: ResultSet)(implicit frs: FromResultSet[A]): String \/ List[A] = {
    import scalaz.syntax.traverse._
    val lf = new ListBuffer[String \/ A]
    \/.fromTryCatchNonFatal(while (rs.next()) lf += frs.fromResultSet(rs, 1)).fold(
      _.getMessage.left,
      _ => lf.toList.sequenceU)
  }
}

class Update1(r: Result[UpdateResult1]) {
  def rowsUpdated: Result[Int] = r.map(_.rowsUpdated)
}

class UpdateResult1(r: Int) {
  def rowsUpdated: Int = r
}

class Update(r: Result[UpdateResult]) {
  def rowsUpdated: Result[Int] = r.flatMap(_.rowsUpdated)
  def returning[A](implicit frs: FromResultSet[A]): Result[A] = r.flatMap(_.returning[A])
}

class UpdateResult(ps: PreparedStatement, r: Int) {

  def rowsUpdated: Result[Int] =
    for {
      _ <- IO(ps).using(_ => IO.ioUnit).liftIO[Result]
    } yield r

  def returning[A](implicit frs: FromResultSet[A]): Result[A] =
    for {
      a <- EitherT(IO(read(ps.getGeneratedKeys)).liftIO[KIOC])
      _ <- IO(ps).using(_ => IO.ioUnit).liftIO[Result]
    } yield a

  private def read[A](rs: ResultSet)(implicit frs: FromResultSet[A]): String \/ A =
    \/.fromTryCatchNonFatal(rs.next()).fold(
      _.getMessage.left,
      _.fold(frs.fromResultSet(rs, 1), "No result found".left))
}

