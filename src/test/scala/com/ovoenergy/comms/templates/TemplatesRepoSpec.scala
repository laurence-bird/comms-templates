package com.ovoenergy.comms.templates

import cats.Id
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.model._
import com.ovoenergy.comms.templates
import com.ovoenergy.comms.templates.cache.CachingStrategy
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, string}
import com.ovoenergy.comms.templates.model.{FileFormat, HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.print.PrintTemplateFiles
import com.ovoenergy.comms.templates.model.template.files.sms.SMSTemplateFiles
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.print.PrintTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.TemplatesRetriever
import org.scalatest.{FlatSpec, Matchers}

class TemplatesRepoSpec extends FlatSpec with Matchers with ValidatedMatchers {

  object NoOpParsing extends Parsing[HandlebarsTemplate] {
    override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
      Invalid(NonEmptyList.of("Some error"))
    }
  }

  val commManifest = CommManifest(Service, "some-template", "1.0")
  val requiredData: Map[String, RequiredTemplateData] = Map(
    "field1" -> string,
    "field2" -> string
  )

  behavior of "Templates Repo"

  it should "return an Invalid if there are no templates for any channels" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               NoOpParsing,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) should
      haveInvalid("Template has no channels defined")
  }

  it should "handle errors retrieving email template" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] =
        Some(Invalid(NonEmptyList.of("Some error retrieving email template")))
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               NoOpParsing,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) should
      haveInvalid("Some error retrieving email template")
  }

  it should "handle errors parsing email template" in {
    val subject  = TemplateFile(commManifest.commType, Email, FileFormat.Text, "The Subject")
    val htmlBody = TemplateFile(commManifest.commType, Email, FileFormat.Html, "The Html Body")
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] =
        Some(
          Valid(
            EmailTemplateFiles(
              subject = subject,
              htmlBody = htmlBody,
              textBody = None,
              sender = None
            )))
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }

    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case s if s == subject  => Invalid(NonEmptyList.of("Error parsing subject"))
          case b if b == htmlBody => Invalid(NonEmptyList.of("Error parsing htmlBody"))
          case _                  => Valid(HandlebarsTemplate("Rendered template", obj(requiredData)))
        }
      }
    }
    TemplatesRepo.getTemplate(
      TemplatesContext(MockTemplatesRetriever,
                       Parser,
                       CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
      commManifest) should (haveInvalid("Error parsing subject") and haveInvalid("Error parsing htmlBody"))
  }

  it should "process valid email template" in {
    val subject  = TemplateFile(commManifest.commType, Email, FileFormat.Text, "The Subject")
    val htmlBody = TemplateFile(commManifest.commType, Email, FileFormat.Html, "The Html Body")
    val other    = TemplateFile(commManifest.commType, Email, FileFormat.Text, "Other")
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] =
        Some(
          Valid(
            EmailTemplateFiles(
              subject = subject,
              htmlBody = htmlBody,
              textBody = Some(other),
              sender = None
            )))
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }

    val parsedSubject  = HandlebarsTemplate("Rendered subject", obj(requiredData))
    val parsedHtmlBody = HandlebarsTemplate("Rendered html body", obj(requiredData))
    val parsedOther    = HandlebarsTemplate("Rendered other", obj(requiredData))
    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case s if s == subject  => Valid(parsedSubject)
          case b if b == htmlBody => Valid(parsedHtmlBody)
          case _                  => Valid(parsedOther)
        }
      }
    }

    val exp = CommTemplate[Id](
      email = Some(EmailTemplate[Id](parsedSubject, parsedHtmlBody, Some(parsedOther), None)),
      sms = None,
      print = None
    )
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               Parser,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) shouldBe Valid(exp)
  }

  it should "handle errors retrieving SMS template" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]] =
        Some(Invalid(NonEmptyList.of("Some error retrieving SMS template")))

      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }

    val result: ErrorsOr[CommTemplate[Id]] =
      TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                                 NoOpParsing,
                                                 CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                                commManifest)
    result should haveInvalid("Some error retrieving SMS template")
  }

  it should "handle errors parsing SMS template" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]] =
        Some(
          Valid(
            SMSTemplateFiles(
              textBody = TemplateFile(commManifest.commType, SMS, FileFormat.Text, "the SMS body")
            )))
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }

    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] =
        Invalid(NonEmptyList.of("Parsing error"))
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               Parser,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) should
      haveInvalid("Parsing error")
  }

  it should "process valid SMS template" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]] =
        Some(
          Valid(
            SMSTemplateFiles(
              textBody = TemplateFile(commManifest.commType, SMS, FileFormat.Text, "the SMS body")
            )))
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] = None
    }

    val parsedBody = HandlebarsTemplate("Rendered SMS body", obj(requiredData))
    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = Valid(parsedBody)
    }

    val exp = CommTemplate[Id](
      email = None,
      sms = Some(SMSTemplate[Id](parsedBody)),
      print = None
    )
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               Parser,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) shouldBe Valid(exp)
  }

  it should "process valid print template" in {
    val header = TemplateFile(commManifest.commType, Print, FileFormat.Text, "The Header")
    val body   = TemplateFile(commManifest.commType, Print, FileFormat.Html, "The Html Body")
    val footer = TemplateFile(commManifest.commType, Print, FileFormat.Text, "Footer")

    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] =
        Some(
          Valid(
            PrintTemplateFiles(body)
          )
        )
    }

    val parsedHeader   = HandlebarsTemplate("Rendered header", obj(requiredData))
    val parsedHtmlBody = HandlebarsTemplate("Rendered html body", obj(requiredData))
    val parsedFooter   = HandlebarsTemplate("Rendered footer", obj(requiredData))

    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case h if h == header => Valid(parsedHeader)
          case b if b == body   => Valid(parsedHtmlBody)
          case f if f == footer => Valid(parsedFooter)
          case _                => fail("Unexpected template file parsed")
        }
      }
    }

    val exp = CommTemplate[Id](
      email = None,
      sms = None,
      print = Some(PrintTemplate[Id](parsedHtmlBody))
    )

    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               Parser,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) shouldBe Valid(exp)
  }

  it should "handle errors retrieving print template" in {
    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] =
        Some(Invalid(NonEmptyList.of("Some error retrieving print template")))
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               NoOpParsing,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) should
      haveInvalid("Some error retrieving print template")
  }

  it should "handle errors parsing print template" in {
    val body = TemplateFile(commManifest.commType, Print, FileFormat.Html, "The Html Body")

    object MockTemplatesRetriever extends TemplatesRetriever {
      override def getEmailTemplate(commManifest: CommManifest): Option[ErrorsOr[EmailTemplateFiles]] = None
      override def getSMSTemplate(commManifest: CommManifest): Option[ErrorsOr[SMSTemplateFiles]]     = None
      override def getPrintTemplate(commManifest: CommManifest): Option[ErrorsOr[PrintTemplateFiles]] =
        Some(
          Valid(
            PrintTemplateFiles(body)
          )
        )
    }

    object Parser extends Parsing[HandlebarsTemplate] {
      override def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
        templateFile match {
          case b if b == body => Invalid(NonEmptyList.of("Error parsing htmlBody"))
          case _              => fail()
        }
      }
    }
    TemplatesRepo.getTemplate(TemplatesContext(MockTemplatesRetriever,
                                               Parser,
                                               CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]),
                              commManifest) should (haveInvalid("Error parsing htmlBody"))
  }

}
