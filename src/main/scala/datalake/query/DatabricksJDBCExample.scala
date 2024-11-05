package datalake.query

import com.opencsv.CSVReader

import scala.collection.JavaConverters._
import play.api.libs.json.{JsString, JsValue, Json}

import java.io.{BufferedWriter, FileReader, FileWriter}
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
    val accountIds = rows.tail.map(row => row(3)).distinct // Dropping the header ro
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

      val results = Iterator
        .continually((resultSet, resultSet.next()))
        .takeWhile(_._2)
        .map { case (rs, _) =>
          (1 to columns).map(i => metaData.getColumnName(i) -> JsString(rs.getString(i))).toMap
        }
        .toList

      val json = Json.toJson(results)
      println(Json.prettyPrint(json)) // Print JSON prettily
      println("number of accountIds with timeouts " + accountIds.size)

      // Convert JSON to a list of maps
      val resultsList: List[Map[String, JsValue]] = Json.fromJson[List[Map[String, JsValue]]](json).get

      // Deduplicate based on invoice.accountId
      val deduplicatedResultsList = resultsList.distinctBy { element =>
        val invoiceField = element("invoice").toString()
        val cleanedInvoiceField = invoiceField
          .replace("\\\"", "\"")
          .replace("\\n", "")
          .replace("\\r", "")
          .replace("\"{", "{")
          .replace("}\"", "}")

        // Extract accountId using substring method
        val accountIdStart = cleanedInvoiceField.indexOf("\"accountId\":\"") + 13
        val accountIdEnd = cleanedInvoiceField.indexOf("\"", accountIdStart)
        val accountId = cleanedInvoiceField.substring(accountIdStart, accountIdEnd)

        accountId
      }
      println("recovered number of accountIds " + deduplicatedResultsList.size)

      // Extract headers
      val headers = deduplicatedResultsList.head.keys.toList

      // Write to CSV
      val csvFile = "output.csv"
      val writer = new BufferedWriter(new FileWriter(csvFile))

      // Write headers
      writer.write(headers.mkString(","))
      writer.newLine()

      // Write rows
      deduplicatedResultsList.foreach { row =>
        val values = headers.map(header => row(header).as[String])
        writer.write(values.mkString(","))
        writer.newLine()
      }

      writer.close()
      resultSet.close()
      statement.close()
      connection.close()
    } catch {
      case e: Exception => e.printStackTrace()
    }

  }
}
