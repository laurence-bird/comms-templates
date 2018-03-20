package com.ovoenergy.comms.templates.model

import com.ovoenergy.comms.model.CustomerAddress

package object variables {

  sealed trait ExpectedCommVariable
  case class ExpectedEmailRecipient(emailAddress: String) extends ExpectedCommVariable

  case class ExpectedSMSRecipient(telephoneNumber: String) extends ExpectedCommVariable

  case class ExpectedPrintRecipient(address: CustomerAddress) extends ExpectedCommVariable

  case class ExpectedSystem(year: String, month: String, dayOfMonth: String) extends ExpectedCommVariable

}
