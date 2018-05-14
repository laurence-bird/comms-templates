package com.ovoenergy.comms.templates.retriever

import com.ovoenergy.comms.model.{CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.print.PrintTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.sms.SMSTemplateFiles

trait TemplatesRetriever {

  def getEmailTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[EmailTemplateFiles]]

  def getSMSTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[SMSTemplateFiles]]

  def getPrintTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[PrintTemplateFiles]]

}
