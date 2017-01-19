package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.Valid
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.RequiredTemplateData._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.Shapeless._

import scala.collection.immutable.Seq
import scala.collection.JavaConverters._

object TemplateParsingProps extends Properties("TemplateParsing") {

  implicit val arbitraryKey: Arbitrary[String] = Arbitrary(Gen.alphaStr.filter(_.nonEmpty))

  implicit def arbitraryNonEmptyMap(implicit
                                    rtd: Arbitrary[RequiredTemplateData])
                                    : Arbitrary[Map[String, RequiredTemplateData]] = Arbitrary {
    Gen.nonEmptyListOf(rtd.arbitrary).flatMap { (values: Seq[RequiredTemplateData]) =>
      val gens: Seq[Gen[(String, RequiredTemplateData)]] = values.map { value =>
        for {
          k <- Gen.alphaStr.filter(_.nonEmpty)
          v <- Gen.const(value)
        } yield (k, v)
      }
      Gen.sequence(gens).map(_.asScala.toList.toMap)
    }
  }

  property("parses valid trees correctly") =
    forAllNoShrink { (tree: obj) =>
      val input = genHandlebarsTemplate(tree, Vector.empty)
      val success =
        TemplateParsing.parseHandlebarsTemplate(input).requiredData == Valid(tree)
      if (!success) {
        println(s"$input ->")
        println(s"  ${TemplateParsing.parseHandlebarsTemplate(input).requiredData}")
        println()
      }
      success
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
