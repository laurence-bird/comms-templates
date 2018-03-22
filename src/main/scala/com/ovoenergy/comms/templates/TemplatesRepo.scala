package com.ovoenergy.comms.templates

import cats.Id
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.model.{EmailSender, HandlebarsTemplate}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.print.PrintTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate

import scala.concurrent.Future
import scala.language.higherKinds

object TemplatesRepo {

  def getTemplate(context: TemplatesContext, commManifest: CommManifest): ErrorsOr[CommTemplate[Id]] =
    context.cachingStrategy.get(commManifest) {
      val commTemplate: CommTemplate[ErrorsOr] = CommTemplate[ErrorsOr](
        email = getEmailTemplate(context, commManifest),
        sms = getSMSTemplate(context, commManifest),
        print = getPrintTemplate(context, commManifest)
      )
      commTemplate.checkAtLeastOneChannelDefined andThen (_ => commTemplate.aggregate)
    }

  private def getEmailTemplate(context: TemplatesContext,
                               commManifest: CommManifest): Option[ErrorsOr[EmailTemplate[Id]]] = {
    val parser: (TemplateFile) => ErrorsOr[HandlebarsTemplate] =
      context.parser.parseTemplate _
    context.templatesRetriever.getEmailTemplate(commManifest).map {
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
                             commManifest: CommManifest): Option[ErrorsOr[SMSTemplate[Id]]] = {
    val parser: (TemplateFile) => ErrorsOr[HandlebarsTemplate] =
      context.parser.parseTemplate _
    context.templatesRetriever.getSMSTemplate(commManifest).map {
      _ andThen { t =>
        val textBody = parser(t.textBody)
        SMSTemplate[ErrorsOr](textBody).aggregate
      }
    }
  }

  private def getPrintTemplate(context: TemplatesContext,
                               commManifest: CommManifest): Option[ErrorsOr[PrintTemplate[Id]]] = {

    val parser: (TemplateFile) => ErrorsOr[HandlebarsTemplate] =
      context.parser.parseTemplate _

    context.templatesRetriever.getPrintTemplate(commManifest).map {
      _ andThen { t =>
        val body = parser(t.body)
        PrintTemplate[ErrorsOr](body).aggregate
      }
    }
  }

}
