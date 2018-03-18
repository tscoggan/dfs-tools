package utils

import java.io.File

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

}