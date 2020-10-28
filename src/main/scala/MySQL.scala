// Copyright (c) 2020. Davi Pereira dos Santos
//     This file is part of the tupi project.
//     Please respect the license. Removing authorship by any means
//     (by code make up or closing the sources) or ignoring property rights
//     is a crime and is unethical regarding the effort and time spent here.
//     Relevant employers or funding agencies will be notified accordingly.
//
//     tupi is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     tupi is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with tupi.  If not, see <http://www.gnu.org/licenses/>.
//
import spire.macros.Auto.java.nio.ByteOrder
import spire.macros.Auto.java.sql.{Connection, DriverManager}

import breeze.linalg.DenseMatrix
import org.json4s.native.JsonParser

import scala.io.Source
/**
 * Cada instancia desta classe representa uma conexao.
 */
class Db() {
  private var connection: Connection = null

  def jsonStrToMap(jsonStr: String): Map[String, Any] = {
    implicit val formats = org.json4s.DefaultFormats

    JsonParser.parse(jsonStr).extract[Map[String, Any]]
  }

  def open() {
    val source = Source.fromFile("config.json")
    val lines = try source.mkString finally source.close()
    val json = jsonStrToMap(lines).asInstanceOf[Map[String, String]]
    try {
      val url = s"jdbc:mysql://" + json("db")
      //      val url = "jdbc:sqlite:////" + database
      connection = DriverManager.getConnection(url, json("user"), json("pass"))
    } catch {
      case e: Throwable => //e.printStackTrace()
        error(s"Problems opening db connection: ${e.getMessage} !") // Trying again in 30s...", 30)
    }
  }

  def error(msg: String) {
    //    if (connection != null && !connection.isClosed) close()
    println(msg)
  }

  def readData(sql: String): List[(Int, String, String, String, String, String)] = {
    val statement = connection.createStatement()
    val resultSet = statement.executeQuery(sql)
    try {
      var lst = List[(Int, String, String, String, String, String)]()
      while (resultSet.next()) {
        val n = resultSet.getInt(1)
        val id = resultSet.getString(2)
        val names = resultSet.getString(3)
        val matrices = resultSet.getString(4)
        val history = resultSet.getString(5)
        val timestamp = resultSet.getString(6)
        lst = lst :+ (n, id, names, matrices, history, timestamp)
      }
      lst
    } catch {
      case e: Throwable => //e.printStackTrace()
        error(s"\nProblems executing SQL query '$sql': ${e.getMessage} .") //\nTrying againg in  $connectionWait_ms ms.\n", 30)
        sys.exit()
    } finally {
      resultSet.close()
      statement.close()
    }
  }

  def readDump(sql: String): List[(Int, String, DenseMatrix[Double])] = {
    val statement = connection.createStatement()
    val resultSet = statement.executeQuery(sql)
    //    try {
    var lst = List[(Int, String, DenseMatrix[Double])]()
    while (resultSet.next()) {
      val n = resultSet.getInt(1)
      val id = resultSet.getString(2)

      val binStream = resultSet.getBinaryStream(3)
      val mark = binStream.readNBytes(1).head

      val buffer = ByteBuffer.wrap(binStream.readNBytes(8)).order(ByteOrder.LITTLE_ENDIAN)
      val (h, w) = (buffer.getInt, buffer.getInt)

      val bytes = binStream.readAllBytes
      val buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).asDoubleBuffer
      val values = new Array[Double](buf.remaining)
      buf.get(values)

      // skip two ints as a double
      val m = new DenseMatrix(h, w, values, offset = 1, majorStride = 0, isTranspose = true)
      lst = lst :+ (n, id, m)
    }
    lst
    //    } catch {
    //      case e: Throwable => //e.printStackTrace()
    //        error(s"\nProblems executing SQL query '$sql': ${e.getMessage} .") //\nTrying againg in  $connectionWait_ms ms.\n", 30)
    //        sys.exit()
    //    } finally {
    resultSet.close()
    statement.close()
    lst
    //    }
  }

  def write(sql: String): Unit = {
    //    test(sql)
    try {
      //      acquire()
      val statement = connection.createStatement()
      statement.executeUpdate(sql)
      statement.close()
    } catch {
      case e: Throwable => //e.printStackTrace()
        val emsg = e.getMessage
        error(s"\nProblems executing SQL query '$sql' in: $emsg}")
      //            if (emsg.contains("Duplicate entry")) error(s"\nProblems executing SQL query '$sql' in: $emsg}")
      //            else log(s"\nProblems executing SQL query '$sql' in: $emsg} .\nTrying againg in  $connectionWait_ms ms", 30)
      //            release()
      //            Thread.sleep(connectionWait_ms)
      //            test(sql)
      //            write(sql)
    } //finally release()
  }


  def writeBlob(sql: String, data: Array[Byte]): Unit = {
    try {
      val statement = connection.prepareStatement(sql)
      statement.setBytes(1, data)
      statement.execute()
      statement.close()
    } catch {
      case e: Throwable => //e.printStackTrace()
        val emsg = e.getMessage
        if (emsg.contains("Duplicate entry")) error(s"\nProblems executing SQL query '$sql' in: $emsg}")
        else error(s"\nProblems executing SQL query '$sql' in: $emsg} .\n") //Trying againg in  $connectionWait_ms ms", 30)
        writeBlob(sql, data)
    } finally {

    }
  }
}

object TestSQL extends App {
  val db = new Db()
  db.open()
  val da = db.readData("select * from data")
  println(da)
  val did = da.head._4.split(",").head
  val (n, id, m) = db.readDump(f"select * from content where id='$did'").head
  print(m)

}