package com.ovoenergy.comms.templates.retriever

import java.util.UUID

import cats.data.NonEmptyList
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.model._
import com.ovoenergy.comms.templates.model.FileFormat
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.print.PrintTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.sms.SMSTemplateFiles
import com.ovoenergy.comms.templates.s3.S3Client
import org.scalatest.{FlatSpec, Matchers}

class TemplatesS3RetrieverSpec extends FlatSpec with Matchers with ValidatedMatchers {

  def s3(contents: Map[String, String], files: Map[String, Seq[String]] = Map.empty) = new S3Client {
    override def listFiles(prefix: String): Seq[String]              = files.getOrElse(prefix, Nil)
    override def getUTF8TextFileContent(key: String): Option[String] = contents.get(key)
  }

  val someHtml         = "<!DOCTYPE html>\n<html>\n<body>\n<div>Random text</div>\n</body>\n</html>"
  val templateManifest = TemplateManifest(UUID.randomUUID().toString.toLowerCase, "0.3")

  behavior of "TemplatesS3Retriever for emails"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/email/subject.txt" -> "the subject",
        s"${templateManifest.id}/0.3/email/body.html"   -> someHtml
      ),
      files = Map(
        s"${templateManifest.id}/0.3/email" -> Seq(
          s"${templateManifest.id}/0.3/email/subject.txt",
          s"${templateManifest.id}/0.3/email/body.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(templateManifest).get should beValid(
      EmailTemplateFiles(
        subject = TemplateFile(Email, FileFormat.Text, "the subject"),
        htmlBody = TemplateFile(Email, FileFormat.Html, someHtml),
        textBody = None,
        sender = None
      ))
  }

  it should "handle template with Sender and text body" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/email/subject.txt" -> "the subject",
        s"${templateManifest.id}/0.3/email/body.html"   -> someHtml,
        s"${templateManifest.id}/0.3/email/body.txt"    -> "text body",
        s"${templateManifest.id}/0.3/email/sender.txt"  -> "Steve <awesome@email.com>"
      ),
      files = Map(
        s"${templateManifest.id}/0.3/email" -> Seq(
          s"${templateManifest.id}/0.3/email/subject.txt",
          s"${templateManifest.id}/0.3/email/body.html",
          s"${templateManifest.id}/0.3/email/body.txt",
          s"${templateManifest.id}/0.3/email/sender.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(templateManifest).get should beValid(
      EmailTemplateFiles(
        subject = TemplateFile(Email, FileFormat.Text, "the subject"),
        htmlBody = TemplateFile(Email, FileFormat.Html, someHtml),
        textBody = Some(TemplateFile(Email, FileFormat.Text, "text body")),
        sender = Some("Steve <awesome@email.com>")
      ))
  }

  it should "handle non existent Template" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.2/email/subject.txt" -> "the subject",
        s"${templateManifest.id}/0.2/email/body.html"   -> someHtml
      ),
      files = Map(
        s"${templateManifest.id}/0.2/email" -> Seq(
          s"${templateManifest.id}/0.2/email/subject.txt",
          s"${templateManifest.id}/0.2/email/body.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(templateManifest) shouldBe None
  }

  it should "handle incomplete template" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/email/body.txt"   -> "text body",
        s"${templateManifest.id}/0.3/email/sender.txt" -> "Steve <awesome@email.com>"
      ),
      files = Map(
        s"${templateManifest.id}/0.3/email" -> Seq(
          s"${templateManifest.id}/0.3/email/body.txt",
          s"${templateManifest.id}/0.3/email/sender.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(templateManifest).get should (haveInvalid(
      "Subject file not found on S3") and haveInvalid("HTML body file not found on S3"))
  }

  behavior of "TemplatesS3Retriever for SMS"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/sms/body.txt" -> "the SMS body"
      ),
      files = Map(
        s"${templateManifest.id}/0.3/sms" -> Seq(
          s"${templateManifest.id}/0.3/sms/body.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getSMSTemplate(templateManifest).get should beValid(
      SMSTemplateFiles(
        textBody = TemplateFile(SMS, FileFormat.Text, "the SMS body")
      ))
  }

  it should "handle non existent Template" in {
    val s3client = s3(
      contents = Map.empty,
      files = Map.empty
    )

    new TemplatesS3Retriever(s3client).getSMSTemplate(templateManifest) shouldBe None
  }

  behavior of "TemplatesS3Retriever for Print"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/print/body.html" -> someHtml
      ),
      files = Map(
        s"${templateManifest.id}/0.3/print" -> Seq(
          s"${templateManifest.id}/0.3/print/body.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(templateManifest).get should beValid(
      PrintTemplateFiles(
        body = TemplateFile(Print, FileFormat.Html, someHtml)
      )
    )
  }

  it should "handle a template with a missing body" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/print/header.html" -> "the header",
        s"${templateManifest.id}/0.3/print/footer.html" -> "the footer"
      ),
      files = Map(
        s"${templateManifest.id}/0.3/print" -> Seq(
          s"${templateManifest.id}/0.3/print/header.html",
          s"${templateManifest.id}/0.3/print/footer.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(templateManifest).get should beInvalid(
      NonEmptyList.of("HTML body file not found on S3")
    )
  }

  it should "handle a template with a missing header" in {
    val s3client = s3(
      contents = Map(
        s"${templateManifest.id}/0.3/print/body.html"   -> someHtml,
        s"${templateManifest.id}/0.3/print/footer.html" -> "the footer"
      ),
      files = Map(
        s"${templateManifest.id}/0.3/print" -> Seq(
          s"${templateManifest.id}/0.3/print/body.html",
          s"  ${templateManifest.id}/0.3/print/footer.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(templateManifest).get should beValid(
      PrintTemplateFiles(
        body = TemplateFile(Print, FileFormat.Html, someHtml)
      )
    )
  }

}
