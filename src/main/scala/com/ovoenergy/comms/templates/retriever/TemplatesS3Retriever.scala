package com.ovoenergy.comms.templates.retriever

import cats.Apply
import cats.data.{NonEmptyList, Validated}
import cats.instances.option._
import cats.implicits._
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
      val body = "body.html"
    }
  }

  def getEmailTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(Email, templateManifest)).isEmpty) {
      log.debug(s"No email template found for $templateManifest")
      None
    } else {
      val subject: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Email.Subject, Email, templateManifest)
          .map(TemplateFile(Email, FileFormat.Text, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("Subject file not found on S3"))
      }

      val htmlBody: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Email.HtmlBody, Email, templateManifest)
          .map(TemplateFile(Email, FileFormat.Html, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("HTML body file not found on S3"))
      }

      val textBody: Option[TemplateFile] = s3File(Filenames.Email.TextBody, Email, templateManifest)
        .map(TemplateFile(Email, FileFormat.Text, _))

      val customSender: Option[String] = s3File(Filenames.Email.Sender, Email, templateManifest)

      Some(
        (subject, htmlBody).mapN {
          case (a, b) =>
            EmailTemplateFiles(a, b, textBody, customSender)
        }
      )
    }
  }

  override def getPrintTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[PrintTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(Print, templateManifest)).isEmpty) {
      log.debug(s"No print template found for $templateManifest")
      None
    } else {

      val body: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.Print.body, Print, templateManifest)
          .map(TemplateFile(Print, FileFormat.Html, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("HTML body file not found on S3"))
      }

      Some(body.map(PrintTemplateFiles))
    }
  }

  override def getSMSTemplate(templateManifest: TemplateManifest): Option[ErrorsOr[SMSTemplateFiles]] = {
    if (s3Client.listFiles(templatePrefix(SMS, templateManifest)).isEmpty) {
      log.debug(s"No SMS template found for $templateManifest")
      None
    } else {
      val textBody: ErrorsOr[TemplateFile] = {
        val option = s3File(Filenames.SMS.TextBody, SMS, templateManifest)
          .map(TemplateFile(SMS, FileFormat.Text, _))
        Validated.fromOption(option, ifNone = NonEmptyList.of("Text body file not found on S3"))
      }
      Some(textBody.map(SMSTemplateFiles))
    }
  }

  private def s3File(filename: String, channel: Channel, templateManifest: TemplateManifest): Option[String] =
    s3Client.getUTF8TextFileContent(templateFileKey(channel, templateManifest, filename))

  private def templatePrefix(channel: Channel, templateManifest: TemplateManifest): String =
    s"${templateManifest.id}/${templateManifest.version}/${channel.toString.toLowerCase}"

  private def templateFileKey(channel: Channel, templateManifest: TemplateManifest, filename: String): String =
    s"${templatePrefix(channel, templateManifest)}/$filename"

}
