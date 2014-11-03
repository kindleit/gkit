package gkit.sql

import java.sql._

case class DB(driver: String, url: String, username: String, password: String) {
  Class.forName(driver)
  def getConnection = DriverManager.getConnection(url, username, password)
}
