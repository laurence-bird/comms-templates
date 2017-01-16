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

  it should "ignore single curly braces" in {
    val input = "hello {there}"
    testValid(input, Map.empty)
  }

  it should "successfully parse a template with some string variables" in {
    val input = "hello {{firstName}} {{lastName}}"
    testValid(input, Map(
      "firstName" -> string,
      "lastName" -> string
    ))
  }

  it should "successfully parse a multi-line template" in {
    val input =
      """
        |hello
        |{{firstName}}
        |{{lastName}}
      """.stripMargin
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

  it should "parse an {{#each}} block representing a sequence of strings" in {
    val input =
      """
        |Hello {{firstName}},
        |You ordered the following things:
        |{{#each things}}
        |  - {{this}}
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "things" -> strings
    ))
  }

  it should "parse an {{#if}} block representing an optional string" in {
    val input =
      """
        |Hello {{firstName}},
        |{{#if order}}
        |  You ordered {{order}}.
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "order" -> optString
    ))
  }

  it should "parse nested variables" in {
    val input = "{{order.amount}}"
    testValid(input, Map("order" -> obj(Map("amount" -> string))))
  }

  it should "parse deeply nested variables" in {
    val input = "{{a.b.c1.d1}} {{a.b.c2.d1}} {{a.b.c1.d2}} {{a.b.c2.d2}}"
    testValid(input, Map(
        "a" -> obj(Map(
          "b" -> obj(Map(
            "c1" -> obj(Map(
              "d1" -> string, "d2" -> string)),
            "c2" -> obj(Map(
              "d1" -> string, "d2" -> string))))))))
  }

  it should "understand 'this' in a nested variable" in {
    val input = "{{this.amount}}"
    testValid(input, Map("amount" -> string))
  }

  it should "parse an {{#each}} block representing a sequence of objects" in {
    val input =
      """
        |Hello {{firstName}},
        |You ordered the following things:
        |{{#each things}}
        |  - {{this.name}} (£{{this.price}})
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "things" -> objs(Map(
        "name" -> string,
        "price" -> string
      ))
    ))
  }

  it should "parse an {{#each}} with a nested key" in {
    val input =
      """
        |Hello {{firstName}},
        |You ordered the following things:
        |{{#each order.items}}
        |  - {{this.name}} (£{{this.price}})
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "order" -> obj(Map(
        "items" -> objs(Map(
          "name" -> string,
          "price" -> string
        ))
      ))
    ))
  }

  it should "parse an {{#if}} block representing an optional object" in {
    val input =
      """
        |Hello {{firstName}},
        |{{#if order}}
        |  You ordered {{order.item}} for £{{order.amount}}.
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "order" -> optObj(Map(
        "item" -> string,
        "amount" -> string
      ))
    ))
  }

  it should "parse a tree of nested {{#each}} and {{#if}} blocks" in {
    pending
    val input =
      """
        |{{#each things}}
        |  {{this.a}}
        |  {{#if this.b}}
        |    {{this.c}}
        |    {{#each this.d}}
        |      {{this.a}}
        |    {{/each}}
        |  {{/if}}
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "things" -> objs(Map(
        "a" -> string,
        "b" -> optString,
        "c" -> string,
        "d" -> objs(Map(
          "a" -> string
        ))
      ))
    ))
  }

  // TODO tests for else blocks

  // TODO tests for invalid templates, e.g. referencing the same thing as both optional and required, or string and map

  private def testValid(input: String, expected: Map[String, RequiredTemplateData]) =
    parseHandlebars(input) should be(Valid(HandlebarsTemplate(input, expected)))

}
