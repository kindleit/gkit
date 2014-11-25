package gkit

import java.sql.Connection

import scalaz._
import scalaz.effect._

package object sql {

  type KIOC[A] = Kleisli[IO, Connection, A]

  type Result[A] = EitherT[KIOC, String, A]
}
