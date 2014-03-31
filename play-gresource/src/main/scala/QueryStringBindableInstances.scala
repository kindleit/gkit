package play.modules.gresource

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.DateTime

import play.api.mvc._

trait QueryStringBindableInstances {

  implicit def DateQueryStringBindable = new QueryStringBindable[Date] {

    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    def parse(s: String): Either[String, Date] =
      try { Right(df.parse(s)) } catch { case e: Throwable => Left(e.getMessage) }

    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Date]] =
      params.get(key).flatMap(_.headOption).map(parse)

    def unbind(key: String, value: Date): String = df.format(value)
  }

  implicit def DateTimeQueryStringBindable = new QueryStringBindable[DateTime] {

    val fmt = ISODateTimeFormat.dateTimeNoMillis

    def parse(s: String): Either[String, DateTime] =
      try { Right(fmt.parseDateTime(s)) } catch { case e: Throwable => Left(e.getMessage) }

    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, DateTime]] =
      params.get(key).flatMap(_.headOption).map(parse)

    def unbind(key: String, value: DateTime): String = fmt.print(value)
  }
}
