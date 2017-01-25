package com.ovoenergy.comms.templates

import cats.Id
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.model.EmailSender
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import org.slf4j.LoggerFactory

import scala.language.higherKinds

object TemplatesRepo {

  private val log = LoggerFactory.getLogger("TemplatesRepo")

  def getTemplate(context: TemplatesContext, commManifest: CommManifest): Option[CommTemplate[ErrorsOr]] = {
    val commTemplate = CommTemplate[ErrorsOr](
      email = getEmailTemplate(context, commManifest)
    )
    validateTemplate(commTemplate) match {
      case Valid(_)  => Some(commTemplate)
      case Invalid(e) =>
        log.warn(s"Template invalid: ${e.toList.mkString(",")}")
        None
    }
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

  private def validateTemplate[A[_]](commTemplate: CommTemplate[A]): ErrorsOr[_] = {
    //At least one channel
   if (commTemplate.email.isEmpty) Invalid(NonEmptyList.of("Template has no channels defined"))
   else Valid(())
  }
}
