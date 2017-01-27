package com.ovoenergy.comms.templates.parsing.handlebars


import cats.Apply
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.github.jknack.handlebars.Handlebars
import com.ovoenergy.comms.model.Channel.{Email, SMS}
import com.ovoenergy.comms.model.CustomerProfile
import com.ovoenergy.comms.templates.model.variables.{EmailRecipient, SMSRecipient, System}
import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, string}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.PartialsRetriever
import org.parboiled2._
import shapeless.LabelledGeneric
import shapeless._
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record._
import shapeless.tag.Tagged

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class HandlebarsParsing(partialsRetriever: PartialsRetriever) extends Parsing[HandlebarsTemplate] {
  private val partialsRegex = "\\{\\{> *([a-zA-Z._]+) *\\}\\}".r
  private val providedDataKeys = Seq("system", "profile", "recipient")

  object symbolName extends Poly1 {
    implicit def atTaggedSymbol[T] = at[Symbol with Tagged[T]](_.name)
  }

  def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
    eitherToErrorsOr {
      for {
        contentIncludingPartials <- resolvePartials(templateFile).right
        _ <- checkTemplateCompiles(contentIncludingPartials).right
      } yield {
        buildRequiredTemplateData(contentIncludingPartials) match {
          case Valid(data) => HandlebarsTemplate(contentIncludingPartials, processRequiredDatas(data, templateFile))
          case invalid     => HandlebarsTemplate(contentIncludingPartials, invalid)
        }
      }
    }
  }

  private[handlebars] def processRequiredDatas(requiredData: RequiredTemplateData.obj, templateFile: TemplateFile): ErrorsOr[RequiredTemplateData.obj] = {
    val systemValidations = validateProvidedDatas(requiredData, "system", classOf[System])
    val profileValidations = validateProvidedDatas(requiredData, "profile", classOf[CustomerProfile])

    val channelSpecificValidations = templateFile.channel match {
      case Email  => validateProvidedDatas(requiredData, "recipient", classOf[EmailRecipient])
      case SMS    => validateProvidedDatas(requiredData, "recipient", classOf[SMSRecipient])
    }

    Apply[ErrorsOr].map3(systemValidations, profileValidations, channelSpecificValidations) {
      case (_, _, _) => RequiredTemplateData.obj(requiredData.fields.filter(field => !providedDataKeys.contains(field._1)))
    }
  }

  /**
    * Parse a template file's content as Handlebars to discover
    * what type of data would be needed to correctly populate it.
    *
    * @param rawExpandedContent The raw content of the Handlebars template,
    *                           with all partials recursively resolved and expanded.
    */
  private[handlebars] def buildRequiredTemplateData(rawExpandedContent: String): ErrorsOr[RequiredTemplateData.obj] = {
    val parser = new HandlebarsASTParser(rawExpandedContent)

    parser.WholeTemplate.run() match {
      case Success(asts) =>
        ASTProcessing.buildRequiredTemplateData(asts)
      case Failure(parseError: ParseError) =>
        Invalid(NonEmptyList.of(parseError.format(parser)))
      case Failure(other) =>
        Invalid(NonEmptyList.of(other.toString))
    }
  }

  private[handlebars] def resolvePartials(templateFile: TemplateFile): Either[String, String] = {
    def retrieveAndReplacePartial(templateContent: String, templateFile: TemplateFile): Either[String, String] = {
      partialsRegex.findFirstMatchIn(templateContent).map(_.group(1)) match {
        case Some(partialName) =>
          val partialContent = partialsRetriever.getSharedPartial(templateFile, partialName)
          partialContent.right.flatMap { c =>
            val processedContent = partialsRegex.replaceFirstIn(templateContent, c)
            retrieveAndReplacePartial(processedContent, templateFile)
          }
        case None => Right(templateContent)
      }
    }
    retrieveAndReplacePartial(templateFile.content, templateFile)
  }

  private[handlebars] def checkTemplateCompiles(input: String): Either[String, Unit] = {
    try {
      new Handlebars().compileInline(input)
      Right(())
    } catch {
      case NonFatal(e) => Left(s"Error compiling template: ${e.getMessage}")
    }
  }

  private def eitherToErrorsOr[A](either: Either[String, A]): ErrorsOr[A] = {
    either match {
      case Right(result) => Valid(result)
      case Left(error)   => Invalid(NonEmptyList.of(error))
    }
  }

  private def validateProvidedDatas[T, R <: HList, M <: HList](requiredData: RequiredTemplateData.obj, providedType: String, clazz: Class[T])
                                                              (implicit labelledGen: LabelledGeneric.Aux[T, R],
                                                               keysR: Keys.Aux[R, M],
                                                               trav: ToTraversable.Aux[M, List, Symbol]): ErrorsOr[_] = {

    val keys = keysR.apply.toList.map(_.name)
    requiredData.fields.get(providedType) match {
      case Some(obj(fields))  =>
        val failures = fields.flatMap {
          case (fieldName, RequiredTemplateData.string) if !keys.contains(fieldName) => Some(s"$providedType.$fieldName is not a valid $providedType property field")
          case (_, RequiredTemplateData.string)                                      => None
          case (fieldName, _)                                                        => Some(s"$providedType.$fieldName is not a string")
        }.toList
        if (failures.nonEmpty) Invalid(NonEmptyList.fromListUnsafe(failures))
        else Valid()
      case Some(otherType)    => Invalid(NonEmptyList.of(s"$providedType property incorrect type"))
      case None               => Valid(())
    }
  }


}
