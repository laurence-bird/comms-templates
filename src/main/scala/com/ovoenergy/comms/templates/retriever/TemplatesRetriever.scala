package com.ovoenergy.comms.templates.retriever

import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.sms.SMSTemplateFiles

trait TemplatesRetriever {

  def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]]

  def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]

}
