package com.ovoenergy.comms.templates

import cats.Id
import com.ovoenergy.comms.model.{CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates.extensions.CommManifestExtensions
import com.ovoenergy.comms.templates.model.{EmailSender, HandlebarsTemplate}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.print.PrintTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate

import scala.language.higherKinds

object TemplatesRepo extends CommManifestExtensions {

  def getTemplate(context: TemplatesContext, commManifest: CommManifest): ErrorsOr[CommTemplate[Id]] = {
    val templateManifest = commManifest.toTemplateManifest
    getTemplateFromManifest(context, templateManifest)
  }

  def getTemplate(context: TemplatesContext, templateManifest: TemplateManifest): ErrorsOr[CommTemplate[Id]] = {
    getTemplateFromManifest(context, templateManifest)
  }

  private def getTemplateFromManifest(context: TemplatesContext, templateManifest: TemplateManifest) = {
    context.cachingStrategy.get(templateManifest) {
      val commTemplate: CommTemplate[ErrorsOr] = CommTemplate[ErrorsOr](
        email = getEmailTemplate(context, templateManifest),
        sms = getSMSTemplate(context, templateManifest),
        print = getPrintTemplate(context, templateManifest)
      )
      commTemplate.checkAtLeastOneChannelDefined andThen (_ => commTemplate.aggregate)
    }
  }

  private def getEmailTemplate(context: TemplatesContext,
                               templateManifest: TemplateManifest): Option[ErrorsOr[EmailTemplate[Id]]] = {
    val parser: (TemplateFile) => ErrorsOr[HandlebarsTemplate] = context.parser.parseTemplate _
    context.templatesRetriever.getEmailTemplate(templateManifest).map {
      _ andThen { t =>
        val subject: ErrorsOr[HandlebarsTemplate] = parser(t.subject)
        val htmlBody                              = parser(t.htmlBody)
        val textBody                              = t.textBody.map(parser(_))
        val sender                                = t.sender.map(EmailSender.parse)
        EmailTemplate[ErrorsOr](subject, htmlBody, textBody, sender).aggregate
      }
    }
  }

  private def getSMSTemplate(context: TemplatesContext,
                             templateManifest: TemplateManifest): Option[ErrorsOr[SMSTemplate[Id]]] = {
    val parser = context.parser.parseTemplate _
    context.templatesRetriever.getSMSTemplate(templateManifest).map {
      _ andThen { t =>
        val textBody = parser(t.textBody)
        SMSTemplate[ErrorsOr](textBody).aggregate
      }
    }
  }

  private def getPrintTemplate(context: TemplatesContext,
                               templateManifest: TemplateManifest): Option[ErrorsOr[PrintTemplate[Id]]] = {

    val parser = context.parser.parseTemplate _
    context.templatesRetriever.getPrintTemplate(templateManifest).map {
      _ andThen { t =>
        val body = parser(t.body)
        PrintTemplate[ErrorsOr](body).aggregate
      }
    }
  }
}
