package com.ovoenergy.comms.templates.retriever

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

  val someHtml     = "<!DOCTYPE html>\n<html>\n<body>\n<div>Random text</div>\n</body>\n</html>"
  val commManifest = CommManifest(Service, "cc-payment-taken", "0.3")

  behavior of "TemplatesS3Retriever for emails"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/email/subject.txt" -> "the subject",
        "service/cc-payment-taken/0.3/email/body.html"   -> someHtml
      ),
      files = Map(
        "service/cc-payment-taken/0.3/email" -> Seq(
          "service/cc-payment-taken/0.3/email/subject.txt",
          "service/cc-payment-taken/0.3/email/body.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(commManifest).get should beValid(
      EmailTemplateFiles(
        subject = TemplateFile(Service, Email, FileFormat.Text, "the subject"),
        htmlBody = TemplateFile(Service, Email, FileFormat.Html, someHtml),
        textBody = None,
        sender = None
      ))
  }

  it should "handle template with Sender and text body" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/email/subject.txt" -> "the subject",
        "service/cc-payment-taken/0.3/email/body.html"   -> someHtml,
        "service/cc-payment-taken/0.3/email/body.txt"    -> "text body",
        "service/cc-payment-taken/0.3/email/sender.txt"  -> "Steve <awesome@email.com>"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/email" -> Seq(
          "service/cc-payment-taken/0.3/email/subject.txt",
          "service/cc-payment-taken/0.3/email/body.html",
          "service/cc-payment-taken/0.3/email/body.txt",
          "service/cc-payment-taken/0.3/email/sender.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(commManifest).get should beValid(
      EmailTemplateFiles(
        subject = TemplateFile(Service, Email, FileFormat.Text, "the subject"),
        htmlBody = TemplateFile(Service, Email, FileFormat.Html, someHtml),
        textBody = Some(TemplateFile(Service, Email, FileFormat.Text, "text body")),
        sender = Some("Steve <awesome@email.com>")
      ))
  }

  it should "handle non existent Template" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.2/email/subject.txt" -> "the subject",
        "service/cc-payment-taken/0.2/email/body.html"   -> someHtml
      ),
      files = Map(
        "service/cc-payment-taken/0.2/email" -> Seq(
          "service/cc-payment-taken/0.2/email/subject.txt",
          "service/cc-payment-taken/0.2/email/body.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(commManifest) shouldBe None
  }

  it should "handle incomplete template" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/email/body.txt"   -> "text body",
        "service/cc-payment-taken/0.3/email/sender.txt" -> "Steve <awesome@email.com>"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/email" -> Seq(
          "service/cc-payment-taken/0.3/email/body.txt",
          "service/cc-payment-taken/0.3/email/sender.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getEmailTemplate(commManifest).get should (haveInvalid(
      "Subject file not found on S3") and haveInvalid("HTML body file not found on S3"))
  }

  behavior of "TemplatesS3Retriever for SMS"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/sms/body.txt" -> "the SMS body"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/sms" -> Seq(
          "service/cc-payment-taken/0.3/sms/body.txt"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getSMSTemplate(commManifest).get should beValid(
      SMSTemplateFiles(
        textBody = TemplateFile(Service, SMS, FileFormat.Text, "the SMS body")
      ))
  }

  it should "handle non existent Template" in {
    val s3client = s3(
      contents = Map.empty,
      files = Map.empty
    )

    new TemplatesS3Retriever(s3client).getSMSTemplate(commManifest) shouldBe None
  }

  behavior of "TemplatesS3Retriever for Print"

  it should "handle basic template" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/post/header.html" -> "the header",
        "service/cc-payment-taken/0.3/post/body.html"   -> someHtml,
        "service/cc-payment-taken/0.3/post/footer.html" -> "the footer"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/post" -> Seq(
          "service/cc-payment-taken/0.3/post/header.html",
          "service/cc-payment-taken/0.3/post/body.html",
          "service/cc-payment-taken/0.3/post/footer.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(commManifest).get should beValid(
      PrintTemplateFiles(
        header = Some(TemplateFile(Service, Post, FileFormat.Text, "the header")),
        body = TemplateFile(Service, Post, FileFormat.Html, someHtml),
        footer = Some(TemplateFile(Service, Post, FileFormat.Text, "the footer"))
      )
    )

  }

  it should "handle a template with a missing body" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/post/header.html" -> "the header",
        "service/cc-payment-taken/0.3/post/footer.html" -> "the footer"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/post" -> Seq(
          "service/cc-payment-taken/0.3/post/header.html",
          "service/cc-payment-taken/0.3/post/footer.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(commManifest).get should beInvalid(
      NonEmptyList.of("HTML body file not found on S3")
    )
  }

  it should "handle a template with a missing header" in {
    val s3client = s3(
      contents = Map(
        "service/cc-payment-taken/0.3/post/body.html"   -> someHtml,
        "service/cc-payment-taken/0.3/post/footer.html" -> "the footer"
      ),
      files = Map(
        "service/cc-payment-taken/0.3/post" -> Seq(
          "service/cc-payment-taken/0.3/post/body.html",
          "service/cc-payment-taken/0.3/post/footer.html"
        )
      )
    )

    new TemplatesS3Retriever(s3client).getPrintTemplate(commManifest).get should beValid(
      PrintTemplateFiles(
        header = None,
        body = TemplateFile(Service, Post, FileFormat.Html, someHtml),
        footer = Some(TemplateFile(Service, Post, FileFormat.Text, "the footer"))
      )
    )
  }

}
