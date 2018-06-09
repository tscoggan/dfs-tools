package utils

object Logger {

  def log(text: String) = println(text)

  def logDebug(text: String) = if (Configs.logDebug) log(text)

  def toHeader(level: Int, text: String): String = mlb.Configs.blogFormat.toUpperCase match {
    case "RG"        => s"h${level}. ${text}"
    case "DRAFTSHOT" => s"<h${level}>${text}</h${level}>"
  }

  def toTable(header: Iterable[Any], rows: Iterable[Iterable[Any]]): String = mlb.Configs.blogFormat.toUpperCase match {
    case "RG" => (header.map(value => s"|_. $value").mkString + "|\n" + rows.map { values => s"|${values.mkString("|")}|" }.mkString("\n"))
      .replaceAll("#", "<notextile>#</notextile>")

    case "DRAFTSHOT" =>
      s"""<div class="table-1">
        |<table width="100%">
        |<thead>
        |  <tr>${header.map(value => s"""<th align="left">${value}</th>""").mkString("")}</tr>
        |</thead>
        |<tbody>
        |  ${rows.map { row => s"""<tr>${row.map(value => s"""<td align="left">${value}</td>""").mkString("")}</tr>""" }.mkString("\n  ")}
        |</tbody>
        |</table>
        |</div>""".stripMargin

    case _ => throw new Exception("Unsupported table format: " + mlb.Configs.blogFormat)
  }
}