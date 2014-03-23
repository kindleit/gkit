package gkit.sql

import java.sql._

abstract class DB {
  def dbms: DBMS
  def getConnection: Connection
}

object DB {
  def apply(_dbms: DBMS, driver: String, url: String, username: String, password: String): DB =
    new DB {
      Class.forName(driver)
      val dbms = _dbms
      def getConnection = DriverManager.getConnection(url, username, password)
    }
}

sealed trait DBMS
case object MySQL extends DBMS
case object PostgreSQL extends DBMS
case object SQLServer98 extends DBMS
