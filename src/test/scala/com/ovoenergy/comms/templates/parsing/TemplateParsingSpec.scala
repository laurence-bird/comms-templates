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

  it should "parse nested {{#each}} blocks" in {
    val input =
      """
        |{{#each orders}}
        |  {{#each this.items}}
        |    {{this.name}} {{this.price}}
        |  {{/each}}
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "orders" -> objs(Map(
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

  it should "parse nested {{#if}} blocks" in {
    val input =
      """
        |{{#if thing}}
        |  {{#if thing.a}}
        |    {{thing.a}} {{thing.b}}
        |  {{/if}}
        |{{/if}}
        |
        |{{#if otherThing}}
        |  {{#if otherThing.a}}
        |    {{otherThing.a.b}}
        |  {{/if}}
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "thing" -> optObj(Map(
        "a" -> optString,
        "b" -> string
      )),
      "otherThing" -> optObj(Map(
        "a" -> optObj(Map("b" -> string))
      ))
    ))
  }

  it should "parse a tree of nested {{#each}} and {{#if}} blocks" in {
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

  it should "parse the most complicated thing I can think of" in {
    val input =
      """
        |{{a}}
        |{{this.b}}
        |{{#each things}}
        |  {{c}}
        |  {{d.a}}
        |  {{this.a.a}}
        |  {{#if this.b.a}}
        |    {{this.c}}
        |    {{#each this.d}}
        |      {{a}}
        |      {{b}}
        |      {{c}}
        |      {{d.a}}
        |      {{d.b}}
        |      {{this.a}}
        |      {{#if this.b.a}}
        |        {{this.b.a}}
        |      {{/if}}
        |      {{#if this.b.c}}
        |        {{this.b.c.d}}
        |      {{/if}}
        |    {{/each}}
        |  {{/if}}
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "a" -> string,
      "b" -> string,
      "c" -> string,
      "d" -> obj(Map("a" -> string, "b" -> string)),
      "things" -> objs(Map(
        "a" -> obj(Map("a" -> string)),
        "b" -> obj(Map("a" -> optString)),
        "c" -> string,
        "d" -> objs(Map(
          "a" -> string,
          "b" -> obj(Map(
            "a" -> optString,
            "c" -> optObj(Map("d" -> string))
          ))
        ))
      ))
    ))
  }

  it should "parse an {{#each}} block representing a sequence of strings, with an else block" in {
    val input =
      """
        |Hello {{firstName}},
        |You ordered the following things:
        |{{#each things}}
        |  - {{this}}
        |{{else}}
        |  You didn't order anything. {{yolo}}!
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "yolo" -> string,
      "things" -> strings
    ))
  }

  it should "parse an {{#each}} block representing a sequence of objects, with an else block" in {
    val input =
      """
        |Hello {{firstName}},
        |You ordered the following things:
        |{{#each things}}
        |  - {{this.name}} (£{{this.price}})
        |{{else}}
        |  You didn't order anything. {{yolo}}!
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "yolo" -> string,
      "things" -> objs(Map(
        "name" -> string,
        "price" -> string
      ))
    ))
  }

  it should "parse an {{#if}} block representing an optional string, with an else block" in {
    val input =
      """
        |Hello {{firstName}},
        |{{#if order}}
        |  You ordered {{order}}.
        |{{else}}
        |  You didn't order anything. {{yolo}}!
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "yolo" -> string,
      "order" -> optString
    ))
  }

  it should "parse an {{#if}} block representing an optional object, with an else block" in {
    val input =
      """
        |Hello {{firstName}},
        |{{#if order}}
        |  You ordered {{order.item}} for £{{order.amount}}.
        |{{else}}
        |  You didn't order anything. {{yolo}}!
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "firstName" -> string,
      "yolo" -> string,
      "order" -> optObj(Map(
        "item" -> string,
        "amount" -> string
      ))
    ))
  }

  it should "recognise a conflict if a field is both a string and an object" in {
    val input =
      """
        |You ordered {{item}}.
        |The amount was {{item.amount}}
      """.stripMargin
    testInvalid(input)
  }

  it should "recognise a conflict if a field is both mandatory and optional" in {
    val input =
      """
        |You ordered {{item}}.
        |{{#if item}}
        |  {{item}}
        |{{/if}}
      """.stripMargin
    testInvalid(input)
  }

  it should "recognise a conflict if a field is both an object and a list" in {
    val input =
      """
        |{{a.b}}.
        |{{#each a}}
        |  {{this.b}}
        |{{/each}}
      """.stripMargin
    testInvalid(input)
  }

  it should "recognise a conflict if a field is both a string and a list" in {
    val input =
      """
        |{{a}}.
        |{{#each a}}
        |  {{#each this.b}}
        |    {{this}}
        |  {{/each}}
        |{{/each}}
      """.stripMargin
    testInvalid(input)
  }

  private def testValid(input: String, expected: Map[String, RequiredTemplateData]) =
    parseHandlebarsTemplate(input) should be(HandlebarsTemplate(input, Valid(obj(expected))))

  private def testInvalid(input: String) =
    parseHandlebarsTemplate(input).requiredData should be('Invalid)

}
