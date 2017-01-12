package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Valid
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import org.scalatest._

class TemplateParsingSpec extends FlatSpec with Matchers {
  import TemplateParsing._
  import RequiredTemplateData._

  it should "successfully parse an empty template" in {
    val input = ""
    testValid(input, Map.empty)
  }

  it should "successfully parse a template with no variables" in {
    val input = "hello"
    testValid(input, Map.empty)
  }

  it should "successfully parse a template with some string variables" in {
    val input = "hello {{firstName}} {{lastName}}"
    testValid(input, Map(
      "firstName" -> string,
      "lastName" -> string
    ))
  }

  it should "successfully parse a template consisting of only a variable" in {
    val input = "{{greeting}}"
    testValid(input, Map("greeting" -> string))
  }

  it should "successfully parse a template ending in raw text" in {
    val input = "hello {{how}} are you"
    testValid(input, Map("how" -> string))
  }

  it should "accept string variables in different valid formats" in {
    val input = "hello {{ firstName  }} {{{lastName}}}"
    testValid(input, Map(
      "firstName" -> string,
      "lastName" -> string
    ))
  }

  private def testValid(input: String, expected: Map[String, RequiredTemplateData]) =
    parseHandlebars(input) should be(Valid(HandlebarsTemplate(input, expected)))

}
