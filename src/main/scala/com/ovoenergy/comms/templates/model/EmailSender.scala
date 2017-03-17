package com.ovoenergy.comms.templates.model

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, _}

case class EmailSender(name: String, emailAddress: String) {
  override def toString = s"$name <$emailAddress>"
}

object EmailSender {

  private val NameAndAddress = """^(.+) <(.+@.+)>$""".r

  def parse(string: String): ValidatedNel[String, EmailSender] = string match {
    case NameAndAddress(name, address) => Valid(EmailSender(name, address))
    case other                         => Invalid(NonEmptyList.of(s"Invalid email sender string: $other"))
  }

}
