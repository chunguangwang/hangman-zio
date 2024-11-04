package datalake.query

import com.opencsv.CSVReader
import scala.collection.JavaConverters._
import play.api.libs.json.{JsString, Json}

import java.io.FileReader
import java.sql.{Connection, DriverManager, ResultSet}
object DatabricksJDBCExample {
  def main(args: Array[String]): Unit = {
    
    val url = args(2)
    args.foreach(println)
    val driver = "com.databricks.client.jdbc.Driver"
    val username = "token"
    val password = args(3)
    val csvReader = new CSVReader(new FileReader(args(4)))
    val rows = csvReader.readAll().asScala
    csvReader.close()
    val accountIds = rows.tail.map(row => row(3)) // Dropping the header ro
    val accountIdsString = accountIds.map(id => s"'$id'").mkString(", ")

    // Query
    val query =
      s"""
        SELECT *
        FROM delta.`s3a://dataeng-data-prod/prod/data/sdp/dss_event_invoicing_invoice_pendingCharge/` AS event
        WHERE event.yr = 2024
          AND event.mo = 9
          AND event.dy > 14
          AND event.dy < 22
          AND event.invoice.accountId IN ($accountIdsString)
      """

    // JDBC connection and query execution
    try {
      Class.forName(driver)
      val connection: Connection = DriverManager.getConnection(url, username, password)
      println("Connection established successfully!")

      val statement = connection.createStatement()
      val resultSet: ResultSet = statement.executeQuery(query)

      // Convert ResultSet to JSON
      val metaData = resultSet.getMetaData
      val columns = metaData.getColumnCount

      val results = Iterator.continually((resultSet, resultSet.next())).takeWhile(_._2).map { case (rs, _) =>
        (1 to columns).map(i => metaData.getColumnName(i) -> JsString(rs.getString(i))).toMap
      }.toList

      val json = Json.toJson(results)
      println(Json.prettyPrint(json)) // Print JSON prettily
      println(results.size)

      resultSet.close()
      statement.close()
      connection.close()
    } catch {
      case e: Exception => e.printStackTrace()
    }

  }
}



