package com.ovoenergy.comms.templates.model

package object variables {

  case class EmailRecipient(emailAddress: String)
  case class SMSRecipient(telephoneNumber: String)

  case class System(year: String, month: String, dayOfMonth: String)

}