package com.ovoenergy.comms.templates.parsing.handlebars

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.github.jknack.handlebars.Handlebars
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.PartialsRetriever
import org.parboiled2._

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class HandlebarsParsing(partialsRetriever: PartialsRetriever) extends Parsing[HandlebarsTemplate] {

  def partialsRegex = "\\{\\{> *([a-zA-Z._]+) *\\}\\}".r

  def parseTemplate(templateFile: TemplateFile): ErrorsOr[HandlebarsTemplate] = {
    eitherToErrorsOr {
      for {
        contentIncludingPartials <- resolvePartials(templateFile).right
        _ <- checkTemplateCompiles(contentIncludingPartials).right
      } yield {
        val requiredData = buildRequiredTemplateData(contentIncludingPartials)
        HandlebarsTemplate(contentIncludingPartials, requiredData)
      }
    }
  }

  /**
    * Parse a template file's content as Handlebars to discover
    * what type of data would be needed to correctly populate it.
    *
    * @param rawExpandedContent The raw content of the Handlebars template,
    *                           with all partials recursively resolved and expanded.
    */
  def buildRequiredTemplateData(rawExpandedContent: String): ErrorsOr[RequiredTemplateData.obj] = {
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

  def resolvePartials(templateFile: TemplateFile): Either[String, String] = {
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

  private def checkTemplateCompiles(input: String): Either[String, Unit] = {
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

}
