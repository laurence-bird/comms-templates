package com.ovoenergy.comms.templates.parsing.handlebars

import cats.Apply
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import com.github.jknack.handlebars.Handlebars
import com.ovoenergy.comms.model.{Email, SMS}
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

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object HandlebarsParsing {
  private val providedDataKeys = Seq("system", "profile", "recipient")

  private[handlebars] def processProvidedDataFields(requiredData: RequiredTemplateData.obj,
                                                    templateFile: TemplateFile): ErrorsOr[RequiredTemplateData.obj] = {
    val systemValidations  = validateProvidedDataType(requiredData, "system", classOf[System])
    val profileValidations = validateProvidedDataType(requiredData, "profile", classOf[CustomerProfile])

    val channelSpecificValidations = templateFile.channel match {
      case Email => validateProvidedDataType(requiredData, "recipient", classOf[EmailRecipient])
      case SMS   => validateProvidedDataType(requiredData, "recipient", classOf[SMSRecipient])
      case _     => Valid(())
    }

    Apply[ErrorsOr].map3(systemValidations, profileValidations, channelSpecificValidations) {
      case (_, _, _) =>
        RequiredTemplateData.obj(requiredData.fields.filter(field => !providedDataKeys.contains(field._1)))
    }
  }

  private def validateProvidedDataType[T, R <: HList, M <: HList](requiredData: RequiredTemplateData.obj,
                                                                  providedType: String,
                                                                  clazz: Class[T])(
      implicit labelledGen: LabelledGeneric.Aux[T, R],
      keysR: Keys.Aux[R, M],
      trav: ToTraversable.Aux[M, List, Symbol]): ErrorsOr[Unit] = {

    val keys = keysR.apply.toList.map(_.name)
    requiredData.fields.get(providedType) match {
      case Some(obj(fields)) =>
        val failures = fields.collect {
          case (fieldName, RequiredTemplateData.string) if !keys.contains(fieldName) =>
            s"$providedType.$fieldName is not a valid $providedType property field"
          case (fieldName, data) if data != RequiredTemplateData.string => s"$providedType.$fieldName is not a string"
        }.toList
        if (failures.nonEmpty) Invalid(NonEmptyList.fromListUnsafe(failures))
        else Valid(())
      case Some(otherType) => Invalid(NonEmptyList.of(s"$providedType property incorrect type"))
      case None            => Valid(())
    }
  }
}

class HandlebarsParsing(partialsRetriever: PartialsRetriever) extends Parsing[HandlebarsTemplate] {
  private val partialsRegex = "\\{\\{> *([a-zA-Z._]+) *\\}\\}".r

  def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
    eitherToErrorsOr {
      for {
        contentIncludingPartials <- resolvePartials(templateFile).right
        _                        <- checkTemplateCompiles(contentIncludingPartials).right
      } yield {
        buildAndValidateRequiredData(contentIncludingPartials, templateFile)
          .map(data => HandlebarsTemplate(contentIncludingPartials, data))
      }
    }
  }

  private def buildAndValidateRequiredData(contentIncludingPartials: String,
                                           templateFile: TemplateFile): ErrorsOr[RequiredTemplateData.obj] = {
    buildRequiredTemplateData(contentIncludingPartials) andThen { data =>
      HandlebarsParsing.processProvidedDataFields(data, templateFile)
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
    val MaxRecursionDepth = 10
    def retrieveAndReplacePartial(templateContent: String,
                                  templateFile: TemplateFile,
                                  depth: Int): Either[String, String] = {
      if (depth >= MaxRecursionDepth) {
        Left("Encountered excessive recursion when expanding partials")
      } else {
        partialsRegex.findFirstMatchIn(templateContent).map(_.group(1)) match {
          case Some(partialName) =>
            // We recurse once to expand any partials referenced within the partial,
            // then recurse again to process the rest of the original input
            for {
              partialContent         <- partialsRetriever.getSharedPartial(templateFile, partialName)
              expandedPartialContent <- retrieveAndReplacePartial(partialContent, templateFile, depth + 1)
              processedContent = partialsRegex.replaceFirstIn(templateContent, expandedPartialContent)
              processRestOfInput <- retrieveAndReplacePartial(processedContent, templateFile, depth)
            } yield processRestOfInput
          case None => Right(templateContent)
        }
      }
    }
    retrieveAndReplacePartial(templateFile.content, templateFile, 0)
  }

  private[handlebars] def checkTemplateCompiles(input: String): Either[String, Unit] = {
    try {
      new Handlebars().compileInline(input)
      Right(())
    } catch {
      case NonFatal(e) => Left(s"Error compiling template: ${e.getMessage}")
    }
  }

  private def eitherToErrorsOr[A](either: Either[String, ErrorsOr[A]]): ErrorsOr[A] = {
    either match {
      case Right(result) => result
      case Left(error)   => Invalid(NonEmptyList.of(error))
    }
  }

}
