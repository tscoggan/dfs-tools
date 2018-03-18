package utils

object Logger {
  
  def log(text: String) = println(text)
  
  def logDebug(text: String) = if (Configs.logDebug) log(text)
  
}