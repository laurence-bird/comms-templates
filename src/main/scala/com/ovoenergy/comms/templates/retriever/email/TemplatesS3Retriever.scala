package com.ovoenergy.comms.templates.retriever.email

import cats.Apply
import cats.data.{NonEmptyList, Validated, _}
import cats.instances.option._
import com.ovoenergy.comms.model.{Channel, CommManifest}
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model._
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.retriever.TemplatesRetriever
import com.ovoenergy.comms.templates.s3.S3Client

class TemplatesS3Retriever(s3Client: S3Client) extends TemplatesRetriever[EmailTemplateFiles] {

  private object Filenames {
    object Email {
      val Subject = "subject.txt"
      val HtmlBody = "body.html"
      val TextBody = "body.txt"
      val Sender = "sender.txt"
    }
  }

  def getTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
    def s3File(filename: String): Option[String] =
      s3Client.getUTF8TextFileContent(emailTemplateFileKey(Channel.Email, commManifest, filename))

    if (s3Client.listFiles(templatePrefix(Channel.Email, commManifest)).isEmpty) {
      None
    } else {
      val subject: ErrorsOr[TemplateFile] =
        Validated.fromOption(s3File(Filenames.Email.Subject).map(TemplateFile(commManifest.commType, Channel.Email, FileFormat.Text, _)),
          ifNone = NonEmptyList.of("Subject file not found on S3"))
      val htmlBody: ErrorsOr[TemplateFile] =
        Validated.fromOption(s3File(Filenames.Email.HtmlBody).map(TemplateFile(commManifest.commType, Channel.Email, FileFormat.Html, _)),
          ifNone = NonEmptyList.of("HTML body file not found on S3"))
      val textBody: Option[TemplateFile] = s3File(Filenames.Email.TextBody).map(TemplateFile(commManifest.commType, Channel.Email, FileFormat.Text, _))
      val customSender: Option[String] = s3File(Filenames.Email.Sender)

      Some(Apply[ErrorsOr]
        .map2(subject, htmlBody) {
          case (sub, html) =>
            EmailTemplateFiles(
              subject = sub,
              htmlBody = html,
              textBody = textBody,
              sender = customSender
            )
        }
      )
    }
  }

  private def templatePrefix(channel: Channel, commManifest: CommManifest): String =
    s"${commManifest.commType.toString.toLowerCase}/${commManifest.name}/${commManifest.version}/${channel.toString.toLowerCase}"

  private def emailTemplateFileKey(channel: Channel, commManifest: CommManifest, filename: String): String =
    s"${templatePrefix(channel, commManifest)}/$filename"






}
