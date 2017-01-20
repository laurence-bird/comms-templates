package com.ovoenergy.comms.templates.model

sealed trait FileFormat {
  def extension: String
}

object FileFormat {
  case object Html extends FileFormat { val extension = "html" }
  case object Text extends FileFormat { val extension = "txt" }
}
