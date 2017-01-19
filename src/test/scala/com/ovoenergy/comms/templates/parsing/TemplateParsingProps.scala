package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Valid
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.RequiredTemplateData._
import org.scalacheck.{Arbitrary, Gen, Properties, Shapeless}
import org.scalacheck.Prop.{BooleanOperators, forAllNoShrink}
import org.scalacheck.Shapeless._

object TemplateParsingProps extends Properties("TemplateParsing") {

  implicit val arbitraryKey: Arbitrary[String] = Arbitrary(Gen.alphaStr.filter(_.nonEmpty))

  property("parses valid trees correctly") =
    forAllNoShrink { (tree: obj) =>
      freeFromEmptyMaps(tree) ==> {
        val input = genHandlebarsTemplate(tree, Vector.empty)
        println(s"$input ->")
        println(s"  ${TemplateParsing.parseHandlebarsTemplate(input).requiredData}")
        println()
        TemplateParsing.parseHandlebarsTemplate(input).requiredData == Valid(tree)
      }
    }

  def freeFromEmptyMaps(tree: RequiredTemplateData): Boolean = tree match {
    case obj(fields) if fields.isEmpty => false
    case objs(fields) if fields.isEmpty => false
    case optObj(fields) if fields.isEmpty => false
    case obj(fields) => fields.values.forall(freeFromEmptyMaps)
    case optObj(fields) => fields.values.forall(freeFromEmptyMaps)
    case objs(fields) => fields.values.forall(freeFromEmptyMaps)
    case _ => true
  }

  def genHandlebarsTemplate(tree: RequiredTemplateData, keyParts: Vector[String]): String = {
    val key = keyParts.mkString(".")
    tree match {
      case `string` => s"{{ $key }}"
      case `optString` => s"{{#if $key }} {{ $key }} {{/if}}"
      case `strings` => s"{{#each $key }} {{ this }} {{/each}}"
      case obj(fields) => fields.map { case (k, v) => genHandlebarsTemplate(v, keyParts :+ k) }.mkString(" ")
      case optObj(fields) =>
        s"{{#if $key }}" +
        fields.map { case (k, v) => genHandlebarsTemplate(v, keyParts :+ k) }.mkString(" ", " ", " ") +
        "{{/if}}"
      case objs(fields) =>
        s"{{#each $key }}" +
        fields.map { case (k, v) => genHandlebarsTemplate(v, Vector("this", k)) }.mkString(" ", " ", " ") +
        "{{/each}}"
    }
  }


}
