package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Valid
import com.ovoenergy.comms.model.Channel.Email
import com.ovoenergy.comms.model.CommType.Service
import com.ovoenergy.comms.templates.model.FileFormat.Html
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData, TemplateFile}
import org.scalatest._

import scala.collection.mutable

class HandlebarsParsingSpec extends FlatSpec with Matchers {
  import HandlebarsParsing._
  import RequiredTemplateData._

  behavior of "#buildRequiredTemplateData"

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

  it should "parse an if inside an each" in {
    val input =
      """
        |{{#each orders }}
        |  {{#if this.amount }} {{ this.amount }}{{/if}}
        |{{/each}}
      """.stripMargin
    testValid(input, Map(
      "orders" -> objs(Map(
        "amount" -> optString))
    ))
  }

  it should "correctly parse this case that scalacheck found" in {
    val input =
      """
        |{{#if wo }}
        |  {{#if wo.rirea }} {{ wo.rirea }} {{/if}}
        |  {{#each wo.udavw }}
        |    {{#if this.oYm.z }} {{ this.oYm.z }} {{/if}}
        |    {{ this.oYm.ug }}
        |    {{#if this.Atjc }} {{ this.Atjc }} {{/if}}
        |    {{#if this.lm.m }} {{ this.lm.m }} {{/if}}
        |    {{#each this.lm.v }} {{ this }} {{/each}}
        |    {{#each this.lm.y }} {{ this }} {{/each}}
        |  {{/each}}
        |  {{#if wo.zabrmp }} {{ wo.zabrmp }} {{/if}}
        |  {{ wo.c }}
        |  {{#if wo.gzcvp }} {{ wo.gzcvp }} {{/if}}
        |{{/if}}
      """.stripMargin
    testValid(input, Map(
      "wo" -> optObj(Map(
        "rirea" -> optString,
        "udavw" -> objs(Map(
          "oYm" -> obj(Map(
            "z" -> optString,
            "ug" -> string)),
          "Atjc" -> optString,
          "lm" -> obj(Map(
            "m" -> optString,
            "v" -> strings,
            "y" -> strings)))),
        "zabrmp" -> optString,
        "c" -> string,
        "gzcvp" -> optString))))
  }

  private def testValid(input: String, expected: Map[String, RequiredTemplateData]) =
    buildRequiredTemplateData(input) should be(Valid(obj(expected)))

  private def testInvalid(input: String) =
    buildRequiredTemplateData(input) should be('Invalid)


  it should "resolve partials with partials" in {

    object partialsRepo extends PartialsRepo {
      val responses: Map[String, Either[String, String]] = Map(
        "a.partial" -> Right("This partial has another partial: {{>  another.partial}}"),
        "another.partial" -> Right("The other partial"),
        "final.partial" -> Right("The final partial"))
      def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String] = {
        responses.getOrElse(partialName, Left("Non mapped response"))
      }
    }
    val templateFile = TemplateFile(Service, Email, Html, "This template contains a partial: {{> a.partial  }}. And a partial at the end: {{>final.partial}}")
    resolvePartials(templateFile, partialsRepo) shouldBe Right("This template contains a partial: This partial has another partial: The other partial. And a partial at the end: The final partial")
  }


  it should "handle partial repo failures" in {

    object partialsRepo extends PartialsRepo {
      def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String] = {
        Left("An error")
      }
    }
    val templateFile = TemplateFile(Service, Email, Html, "This template contains a partial: {{> a.partial}}")
    resolvePartials(templateFile, partialsRepo) shouldBe Left("An error")
  }

}
