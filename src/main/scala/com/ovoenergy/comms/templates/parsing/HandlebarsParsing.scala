package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Invalid
import cats.data.{Kleisli, NonEmptyList, ReaderT}
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import org.parboiled2._

import scala.util.{Failure, Success}

object HandlebarsParsing {

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

  def parseHandlebarsTemplate(input: String) = ReaderT[ErrorsOr, PartialsRepo, HandlebarsTemplate] {
    for {
      contentIncludingPartials <- resolvePartials(input)
      _ <- Kleisli.pure(checkTemplateCompiles(contentIncludingPartials))
      requiredData <- Kleisli.pure(buildRequiredTemplateData(contentIncludingPartials))
    } yield {
      HandlebarsTemplate(contentIncludingPartials, requiredData)
    }
  }

  def resolvePartials(input: String) = ReaderT[ErrorsOr, PartialsRepo, String] { partialsRepo =>
    ???
  }

  def checkTemplateCompiles(input: String): ErrorsOr[Unit] = {
    // TODO try compiling with handlebars
    Unit
  }

}
