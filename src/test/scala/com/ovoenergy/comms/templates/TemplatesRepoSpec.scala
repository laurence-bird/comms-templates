package com.ovoenergy.comms.templates

import cats.Id
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.ovoenergy.comms.model.{Channel, CommManifest, CommType}
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, string}
import com.ovoenergy.comms.templates.model.{FileFormat, HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.TemplatesRetriever
import org.scalatest.{FlatSpec, Matchers}

class TemplatesRepoSpec extends FlatSpec
  with Matchers {

  object NoOpParsing extends Parsing[HandlebarsTemplate] {
    override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
      Invalid(NonEmptyList.of("Some error"))
    }
  }

  val commManifest = CommManifest(CommType.Service, "some-template", "1.0")
  val requiredData: Map[String, RequiredTemplateData] =  Map(
    "field1" -> string,
    "field2" -> string
  )

  behavior of "Templates Repo"

  it should "handle email template not existing" in {
    object MockEmailTemplatesRetriever extends TemplatesRetriever[EmailTemplateFiles] {
      override def getTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
        None
      }
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockEmailTemplatesRetriever, NoOpParsing), commManifest) shouldBe None
  }

  it should "handle errors retrieving email template" in {
    object MockEmailTemplatesRetriever extends TemplatesRetriever[EmailTemplateFiles] {
      override def getTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
        Some(Invalid(NonEmptyList.of("Some error retrieving template")))
      }
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockEmailTemplatesRetriever, NoOpParsing), commManifest).get.email shouldBe Some(Invalid(NonEmptyList.of("Some error retrieving template")))
  }

  it should "handle errors parsing email template" in {
    val subject = TemplateFile(commManifest.commType, Channel.Email, FileFormat.Text, "The Subject")
    val htmlBody = TemplateFile(commManifest.commType, Channel.Email, FileFormat.Html, "The Html Body")
    object MockEmailTemplatesRetriever extends TemplatesRetriever[EmailTemplateFiles] {
      override def getTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
        Some(Valid(EmailTemplateFiles(
          subject = subject,
          htmlBody = htmlBody,
          textBody = None,
          sender = None
        )))
      }
    }

    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case s if s == subject  => Invalid(NonEmptyList.of("Error parsing subject"))
          case b if b == htmlBody => Invalid(NonEmptyList.of("Error parsing htmlBody"))
          case _                  => Valid(HandlebarsTemplate("Rendered template", Valid(obj(requiredData))))
        }
      }
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockEmailTemplatesRetriever, Parser), commManifest).get.email shouldBe Some(Invalid(NonEmptyList.of("Error parsing subject", "Error parsing htmlBody")))
  }

  it should "process valid email template" in {
    val subject = TemplateFile(commManifest.commType, Channel.Email, FileFormat.Text, "The Subject")
    val htmlBody = TemplateFile(commManifest.commType, Channel.Email, FileFormat.Html, "The Html Body")
    val other = TemplateFile(commManifest.commType, Channel.Email, FileFormat.Text, "Other")
    object MockEmailTemplatesRetriever extends TemplatesRetriever[EmailTemplateFiles] {
      override def getTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = {
        Some(Valid(EmailTemplateFiles(
          subject = subject,
          htmlBody = htmlBody,
          textBody = Some(other),
          sender = None
        )))
      }
    }

    val parsedSubject = HandlebarsTemplate("Rendered subject", Valid(obj(requiredData)))
    val parsedHtmlBody = HandlebarsTemplate("Rendered html body", Valid(obj(requiredData)))
    val parsedOther = HandlebarsTemplate("Rendered other", Valid(obj(requiredData)))
    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case s if s == subject  => Valid(parsedSubject)
          case b if b == htmlBody => Valid(parsedHtmlBody)
          case _                  => Valid(parsedOther)
        }
      }
    }

    val exp = CommTemplate[ErrorsOr](
      email = Some(Valid(EmailTemplate[Id](parsedSubject, parsedHtmlBody, Some(parsedOther), None)))
    )
    TemplatesRepo.getTemplate(TemplatesContext(MockEmailTemplatesRetriever, Parser), commManifest) shouldBe Some(exp)
  }



}
