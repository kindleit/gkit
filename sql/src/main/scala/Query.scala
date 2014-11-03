package gkit.sql

import java.sql._

import scala.collection.mutable.ListBuffer

import scalaz.effect._

import scalaz.std.effect.AllEffectInstances._
import scalaz.std.list._
import scalaz.std.option._

import scalaz.syntax.effect.all._
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._

import scalaz._

object Queries {

  type KIOC[A] = Kleisli[IO, Connection, A]

  class ExecuteResult(s: PreparedStatement, srs: String \/ ResultSet) {

    def first[A](implicit frs: FromResultSet[A]): Kleisli[IO, Connection, String \/ Option[A]] =
      for {
        a  <- IO(srs.fold(_.left, rs => rs.next().fold(frs.fromResultSet(rs, 1).map(Some(_)), None.right))).liftIO[KIOC]
        _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
      } yield a

    def toList[A](implicit frs: FromResultSet[A]): Kleisli[IO, Connection, String \/ List[A]] =
      for {
        as <- srs.fold(e => IO(e.left), rs => readAll(rs)).liftIO[KIOC]
        _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
      } yield as
  }

  def delete(q: String): Kleisli[IO, Connection, Int] = for {
    s <- Kleisli.ask[IO, Connection].map(_.createStatement)
    r <- IO(s.executeUpdate(q)).liftIO[KIOC]
    _ <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield r

  def readAll[A](rs: ResultSet)(implicit frs: FromResultSet[A]) = IO {
    import scalaz.syntax.traverse._
    val lf = new ListBuffer[String \/ A]
    while (rs.next()) lf += frs.fromResultSet(rs, 1)
    lf.toList.sequenceU
  }

  def execute[A](q: String)(a: A)(implicit ts: ToStatement[A]): Kleisli[IO, Connection, ExecuteResult] = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(q))
    fr <- IO(ts.toStatement(a, s)).liftIO[KIOC]
    rs <- fr.traverse(s => IO(s.executeQuery()).liftIO[KIOC])
  } yield new ExecuteResult(s, rs)

  def execute1(q: String): Kleisli[IO, Connection, ExecuteResult] = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(q))
    rs <- IO(s.executeQuery()).liftIO[KIOC]
  } yield new ExecuteResult(s, rs.right[String])

  def update[A](q: String)(a: A)(implicit ts: ToStatement[A]) = for {
    s  <- Kleisli.ask[IO, Connection].map(_.prepareStatement(q))
    fr <- IO(ts.toStatement(a, s)).liftIO[KIOC]
    er <- fr.traverse(s => IO(s.executeUpdate()).liftIO[KIOC])
    _  <- IO(s).using(_ => IO.ioUnit).liftIO[KIOC]
  } yield er
}
