package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Invalid
import cats.data.NonEmptyList
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import org.parboiled2._

import scala.util.{Failure, Success}

object TemplateParsing {

  /**
    * Parse a Handlebars template to discover what type of data would be needed to correctly populate it.
    */
  def parseHandlebars(input: String): ErrorsOr[HandlebarsTemplate] = {
    val parser = new HandlebarsASTParser(input)

    parser.WholeTemplate.run() match {
      case Success(asts) =>
        ASTProcessing.buildRequiredTemplateData(asts)
          .map(HandlebarsTemplate(input, _))
      case Failure(parseError: ParseError) =>
        Invalid(NonEmptyList.of(parseError.format(parser)))
      case Failure(other) =>
        Invalid(NonEmptyList.of(other.toString))
    }
  }

}
