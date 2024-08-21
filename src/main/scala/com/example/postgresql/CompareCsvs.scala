package com.example.postgresql

import java.io.{BufferedReader, FileReader, IOException}
import scala.collection.mutable

object CompareCsvs extends App {
  val set_01_04 = getUserIds(
    "/Users/chunguang.wang/MyDocuments/Hulu/invoice/extract-2024-01-04T23_16_23.210Z_sprint_not_switch_users_unique_user_ids.csv"
  )
  val set_01_09 = getUserIds(
    "/Users/chunguang.wang/MyDocuments/Hulu/invoice/extract-2024-01-09T03_07_04.340Z_sprint_not_switch_users_unique_user_ids.csv"
  )
  println(set_01_09.size + "  " + set_01_09)

  val innerSet = mutable.Set[String]()
  val set_01_04_outer = mutable.Set[String]()
  val set_01_09_outer = mutable.Set[String]()
  for (userId_01_04 <- set_01_04) {
    if (set_01_09.contains(userId_01_04)) {
      innerSet.add(userId_01_04)
    } else {
      set_01_04_outer.add(userId_01_04)
    }
  }

  for (userId_01_09 <- set_01_09) {
    if (set_01_04.contains(userId_01_09)) {
      innerSet.add(userId_01_09)
    } else {
      set_01_09_outer.add(userId_01_09)
    }
  }
  println(innerSet.size + "  " + innerSet)
  println(set_01_04_outer.size + "  " + set_01_04_outer)
  println(set_01_09_outer.size + "  " + set_01_09_outer)

  private def getUserIds(pathOfCsv: String): mutable.Set[String] = {
    val columnNumber =
      1 // Specify the column number to concatenate (e.g., 1 for the first column)

    val delimiter =
      "," // Specify the delimiter used in the CSV file (e.g., comma)

    val inputFile = pathOfCsv
    val reader = new BufferedReader(new FileReader(inputFile))
    val concatenatedColumn = scala.collection.mutable.Set[String]()

    try {
      var line: String = reader.readLine
      while (line != null) {
        val columns = line.split(delimiter)
        // Check if the column number is valid
        if (columnNumber > 0 && columnNumber <= columns.length) {
          val columnValue = columns(columnNumber - 1).trim
          concatenatedColumn.add(columnValue)
        }
        line = reader.readLine
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    } finally if (reader != null) reader.close()
    concatenatedColumn

  }
}
