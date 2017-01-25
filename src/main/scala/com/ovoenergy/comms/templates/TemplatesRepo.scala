package com.ovoenergy.comms.templates

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.model.EmailSender
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate

import scala.language.higherKinds

object TemplatesRepo {

  def getTemplate(context: TemplatesContext, commManifest: CommManifest): Option[CommTemplate[ErrorsOr]] = {
    val commTemplate = CommTemplate[ErrorsOr](
      email = getEmailTemplate(context, commManifest)
    )
    if (isValidTemplate(commTemplate)) Some(commTemplate)
    else None
  }

  private def getEmailTemplate(context: TemplatesContext, commManifest: CommManifest): Option[ErrorsOr[EmailTemplate[Id]]] = {
    val parser = context.parser.parseTemplate _
    context.emailTemplateRetriever.getTemplate(commManifest).map {
      case Valid(t) =>
        val subject = parser(t.subject)
        val htmlBody = parser(t.htmlBody)
        val textBody = t.textBody.map(parser(_))
        val sender = t.sender.map(EmailSender.parse)
        EmailTemplate[ErrorsOr](subject, htmlBody, textBody, sender).aggregate
      case Invalid(e) => Invalid(e)
    }
  }

  private def isValidTemplate[A[_]](commTemplate: CommTemplate[A]) = {
    //At least one channel
    commTemplate.email.isDefined
  }
}
