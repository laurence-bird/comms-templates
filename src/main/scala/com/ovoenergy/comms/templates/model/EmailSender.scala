package com.ovoenergy.comms.templates.model

case class EmailSender(name: String, emailAddress: String) {
  override def toString = s"$name <$emailAddress>"
}

