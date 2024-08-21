package com.example.postgresql

import java.io.{BufferedReader, FileReader, IOException, PrintWriter}
import java.sql.{
  Connection,
  DriverManager,
  PreparedStatement,
  ResultSet,
  SQLException
}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.break

object PostgresqlJdbcConnector {
  def main(args: Array[String]): Unit = {

    // Database connection parameters
    val url =
      "jdbc:postgresql://jive-async-request-master.prod.hulu.com:5432/jive_async_request_v2"
    val user: String = "jive_async_request_app"
    val password: String = "iLJaVbJXBm5v"

    // Establish a connection
    var connection: Connection = null
    var statement: PreparedStatement = null
    var resultSet: ResultSet = null
    try {
      connection = DriverManager.getConnection(url, user, password)
      println("Connected to the PostgreSQL server successfully.")
      val sql =
        "SELECT * FROM subscriber_failed_async_request WHERE user_id = ?";
      statement = connection.prepareStatement(sql)
      val userIds = getArray()

      // Set the value for the user_id parameter
      val userId = 159020861; // Replace with the actual user ID
      statement.setInt(1, userId);

      // Execute the query
      resultSet = statement.executeQuery()
      val csvFile = "query_results.csv"
      val writer = new PrintWriter(csvFile)
      // Write the column headers to the CSV file
      val metaData = resultSet.getMetaData
      val numColumns = metaData.getColumnCount
      for (i <- 1 to numColumns) {
        writer.print(metaData.getColumnName(i))
        if (i < numColumns) writer.print(",")
        else writer.println()
      }

      for (user_id <- userIds) {
        println(user_id)
        statement.setInt(1,  user_id.toInt)

        // Execute the query
        resultSet = statement.executeQuery()

        while (resultSet.next()) {
          // Process the results here
          for (i <- 1 to numColumns) {
            writer.print(resultSet.getString(i))
            if (i < numColumns) writer.print(",")
            else writer.println()
          }
        }
      }
      writer.close()
      println(s"Query results have been written to $csvFile")
    } catch {
      case e: SQLException =>
        println("SQL Exception: " + e.getMessage());
    } finally {
      if (resultSet != null) resultSet.close()
      if (statement != null) statement.close()
      if (connection != null) connection.close()
    }
  }

  private def getArray(): List[String] = {
    val columnNumber =
      1 // Specify the column number to concatenate (e.g., 1 for the first column)

    val delimiter =
      "," // Specify the delimiter used in the CSV file (e.g., comma)

    val inputFile =
      "/Users/chunguang.wang/MyDocuments/Hulu/invoice/extract-2024-01-04T23_16_23.210Z_sprint_not_switch_users.csv"

    val reader = new BufferedReader(new FileReader(inputFile))
    val concatenatedColumn = ArrayBuffer[String]()

    try {
      var line: String = reader.readLine
      while (line != null) {
        val columns = line.split(delimiter)
        // Check if the column number is valid
        if (columnNumber > 0 && columnNumber <= columns.length) {
          val columnValue = columns(columnNumber - 1).trim
          concatenatedColumn.append(columnValue)
        }
        line = reader.readLine
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    } finally if (reader != null) reader.close()
    concatenatedColumn.remove(0)
    concatenatedColumn.toList
  }
}
