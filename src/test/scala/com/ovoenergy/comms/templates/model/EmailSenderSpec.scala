package com.ovoenergy.comms.templates.model

import cats.scalatest.ValidatedMatchers
import org.scalatest.{FlatSpec, Matchers}

class EmailSenderSpec extends FlatSpec with Matchers with ValidatedMatchers {

  behavior of "Email Sender"

  it should "parse a valid sender" in {
    EmailSender.parse("Steve <awesome@email.com>") should beValid(EmailSender("Steve", "awesome@email.com"))
  }

  it should "error when invalid sender parsed" in {
    EmailSender.parse("Steve <oops>") should haveInvalid("Invalid email sender string: Steve <oops>")
  }

}
