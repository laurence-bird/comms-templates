package com.ovoenergy.comms.templates.retriever

import cats.Apply
import cats.data.{NonEmptyList, Validated}
import cats.instances.option._
import com.ovoenergy.comms.model._
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model._
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.print.PrintTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.sms.SMSTemplateFiles
import com.ovoenergy.comms.templates.s3.S3Client
import org.slf4j.LoggerFactory

class TemplatesS3Retriever(s3Client: S3Client) extends TemplatesRetriever {

  private val log = LoggerFactory.getLogger("TemplatesS3Retriever")

  private object Filenames {
    object Email {
      val Subject  = "subject.txt"
      val HtmlBody = "body.html"
      val TextBody = "body.txt"
      val Sender   = "sender.txt"
    }
    object SMS {
      val TextBody = "body.txt"
    }
    object Print {
      val header = "header.html"
      val body   = "body.html"
      val footer = "footer.html"
    }
  }

  def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(Email, commManifest)).isEmpty) {
      log.debug(s"No email template found for $commManifest")
      None
    } else {
      val subject: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Email.Subject, Email, commManifest)
          .map(TemplateFile(commManifest.commType, Email, FileFormat.Text, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("Subject file not found on S3"))
      }

      val htmlBody: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Email.HtmlBody, Email, commManifest)
          .map(TemplateFile(commManifest.commType, Email, FileFormat.Html, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("HTML body file not found on S3"))
      }

      val textBody: Option[TemplateFile] = s3File(Filenames.Email.TextBody, Email, commManifest)
        .map(TemplateFile(commManifest.commType, Email, FileFormat.Text, _))

      val customSender: Option[String] = s3File(Filenames.Email.Sender, Email, commManifest)

      Some(
        Apply[ErrorsOr]
          .map2(subject, htmlBody) {
            case (sub, html) =>
              EmailTemplateFiles(
                subject = sub,
                htmlBody = html,
                textBody = textBody,
                sender = customSender
              )
          })
    }
  }

  override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(Post, commManifest)).isEmpty) {
      log.debug(s"No print template found for $commManifest") // TODO: update comms kafka messages, change post to print
      None
    } else {

      val header: Option[TemplateFile] = {
        s3File(Filenames.Print.header, Post, commManifest)
          .map(TemplateFile(commManifest.commType, Post, FileFormat.Text, _))
      }

      val body: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Print.body, Post, commManifest)
          .map(TemplateFile(commManifest.commType, Post, FileFormat.Html, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("HTML body file not found on S3"))
      }

      val footer: Option[TemplateFile] = {
        s3File(Filenames.Print.footer, Post, commManifest)
          .map(TemplateFile(commManifest.commType, Post, FileFormat.Text, _))
      }

      Some(
        Apply[ErrorsOr]
          .map(body) {
            case (b) =>
              PrintTemplateFiles(
                body = b,
                header = header,
                footer = footer
              )
          })
    }
  }

  override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(SMS, commManifest)).isEmpty) {
      log.debug(s"No SMS template found for $commManifest")
      None
    } else {
      val textBody: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.SMS.TextBody, SMS, commManifest)
          .map(TemplateFile(commManifest.commType, SMS, FileFormat.Text, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("Text body file not found on S3"))
      }
      Some(textBody.map(SMSTemplateFiles))
    }
  }

  private def s3File(filename: String, channel: Channel, commManifest: CommManifest): Option[String] =
    s3Client.getUTF8TextFileContent(templateFileKey(channel, commManifest, filename))

  private def templatePrefix(channel: Channel, commManifest: CommManifest): String =
    s"${commManifest.commType.toString.toLowerCase}/${commManifest.name}/${commManifest.version}/${channel.toString.toLowerCase}"

  private def templateFileKey(channel: Channel, commManifest: CommManifest, filename: String): String =
    s"${templatePrefix(channel, commManifest)}/$filename"

}
