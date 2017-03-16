package com.ovoenergy.comms.templates

import cats.Id
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.model.EmailSender
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate

import scala.language.higherKinds

object TemplatesRepo {

  def getTemplate(context: TemplatesContext, commManifest: CommManifest): ErrorsOr[CommTemplate[Id]] = {
    val commTemplate = CommTemplate[ErrorsOr](
      email = getEmailTemplate(context, commManifest),
      sms = getSMSTemplate(context, commManifest)
    )
    commTemplate.checkAtLeastOneChannelDefined andThen (_ => commTemplate.aggregate)
  }

  private def getEmailTemplate(context: TemplatesContext, commManifest: CommManifest): Option[ErrorsOr[EmailTemplate[Id]]] = {
    val parser = context.parser.parseTemplate _
    context.templatesRetriever.getEmailTemplate(commManifest).map {
      _ andThen { t =>
        val subject = parser(t.subject)
        val htmlBody = parser(t.htmlBody)
        val textBody = t.textBody.map(parser(_))
        val sender = t.sender.map(EmailSender.parse)
        EmailTemplate[ErrorsOr](subject, htmlBody, textBody, sender).aggregate
      }
    }
  }

  private def getSMSTemplate(context: TemplatesContext, commManifest: CommManifest): Option[ErrorsOr[SMSTemplate[Id]]] = {
    val parser = context.parser.parseTemplate _
    context.templatesRetriever.getSMSTemplate(commManifest).map {
      _ andThen { t =>
        val textBody = parser(t.textBody)
        SMSTemplate[ErrorsOr](textBody).aggregate
      }
    }
  }

}
