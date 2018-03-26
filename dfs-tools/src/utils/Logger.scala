package utils

object Logger {

  def log(text: String) = println(text)

  def logDebug(text: String) = if (Configs.logDebug) log(text)

  def toHtmlTable(header: Iterable[Any], rows: Iterable[Iterable[Any]]): String = {
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
  }
}