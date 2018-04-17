package utils

import java.io.{ File, FileWriter }
import scala.collection.mutable
import Logger._

object FileUtils {

  /**
   * Returns a list of all files in the specifier directory with the specified extension(s).  If no extensions are
   * specified, all files are returned.
   */
  def getListOfFiles(dir: String, extensions: String*): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      if (extensions.isEmpty) d.listFiles.filter(_.isFile).toList
      else {
        d.listFiles.filter(_.isFile).toList.filter { f =>
          extensions.toList.exists { e => f.getName.toUpperCase.endsWith(e.toUpperCase) }
        }
      }
    } else Nil
  }

  private val fileWriterCache: mutable.Map[String, FileWriter] = mutable.Map.empty

  private def getFileWriter(fileName: String): FileWriter = synchronized {
    fileWriterCache.get(fileName) match {
      case Some(cached) => cached
      case None => {
        log(s"$fileName file writer not found in cache --> adding")
        val fw = new FileWriter(fileName, true)
        fileWriterCache += (fileName -> fw)
        fw
      }
    }
  }

  def writeToFile(s: String, fileName: String, overwrite: Boolean = false): Unit = {
    val file = new File(fileName)
    if (overwrite && file.exists) file.delete
    val writer = getFileWriter(fileName)
    writer.write(s)
    writer.flush
  }

  def writeLinesToFile(lines: Iterable[String], fileName: String, overwrite: Boolean = false): Unit = {
    val file = new File(fileName)
    if (overwrite && file.exists) file.delete
    val writer = getFileWriter(fileName)
    lines.foreach { line => writer.write(line + "\n") }
    writer.flush
  }

  def fileExists(fileName: String): Boolean = (new File(fileName)).exists

}