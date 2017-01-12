package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import org.parboiled2._

import scala.util.{Failure, Success}

object TemplateParsing {
  import RequiredTemplateData._

  private class TemplateParser(val input: ParserInput) extends Parser {

    def RawText = rule { zeroOrMore((! str("{{")) ~ ANY) }

    def VariableName = rule { oneOrMore(CharPredicate.AlphaNum | '_' | '.') }

    def SimpleDoubleCurlies = rule { "{{" ~ zeroOrMore(" ") ~ capture(VariableName) ~ zeroOrMore(" ") ~ "}}" }
    def SimpleTripleCurlies = rule { "{{{" ~ zeroOrMore(" ") ~ capture(VariableName) ~ zeroOrMore(" ") ~ "}}}" }

    def Block = rule { SimpleDoubleCurlies | SimpleTripleCurlies } // TODO more complex rules

    def Template = rule { RawText ~ zeroOrMore(Block).separatedBy(RawText) ~ RawText ~ EOI }

  }

  /**
    * Parse a Handlebars template to discover what type of data would be needed to correctly populate it.
    */
  def parseHandlebars(input: String): ValidatedNel[String, HandlebarsTemplate] = {
    new TemplateParser(input).Template.run() match {
      case Success(variableNames) =>
        val requiredTemplateData = variableNames.map(k => (k, string)).toMap
        Valid(HandlebarsTemplate(input, requiredTemplateData))
      case Failure(e) => Invalid(NonEmptyList.of(e.toString))
    }
  }

}
