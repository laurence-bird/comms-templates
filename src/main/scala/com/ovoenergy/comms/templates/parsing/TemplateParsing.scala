package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.traverse._
import cats.instances.list._
import com.ovoenergy.comms.templates.model.{ErrorsOr, HandlebarsTemplate, RequiredTemplateData}
import org.parboiled2._

import scala.util.{Failure, Success}

object TemplateParsing {
  import RequiredTemplateData._

  private sealed trait AST
  private object AST {
    case class Ref(text: String) extends AST
    case class Each(text: String, children: Seq[AST]) extends AST
    case class If(text: String, children: Seq[AST]) extends AST
  }

  private class TemplateParser(val input: ParserInput) extends Parser {

    def WS: Rule0 = rule { zeroOrMore(" ") }

    def RawText: Rule0 = rule { zeroOrMore((! str("{{")) ~ ANY) }

    def RefText: Rule0 = rule { oneOrMore(CharPredicate.AlphaNum | '_' | '.') }

    def Ref: Rule1[AST] = rule { capture(RefText) ~> ((text: String) => AST.Ref(text)) }

    def SimpleDoubleCurlies: Rule1[AST] = rule {
      "{{" ~ WS ~ Ref ~ WS ~ "}}"
    }

    def SimpleTripleCurlies: Rule1[AST] = rule {
      "{{{" ~ WS ~ Ref ~ WS ~ "}}}"
    }

    // TODO optional else block
    def StartEach: Rule1[String] = rule { "{{" ~ WS ~ "#each " ~ capture(RefText) ~ WS ~ "}}" }
    def EndEach: Rule0 = rule { "{{" ~ WS ~ "/each" ~ WS ~ "}}" }
    def Each: Rule1[AST] = rule {
      (StartEach ~ Template ~ EndEach) ~> ((refText: String, children: Seq[AST]) => AST.Each(refText, children))
    }

    // TODO optional else block
    def StartIf: Rule1[String] = rule { "{{" ~ WS ~ "#if " ~ capture(RefText) ~ WS ~ "}}" }
    def EndIf: Rule0 = rule { "{{" ~ WS ~ "/if" ~ WS ~ "}}" }
    def If: Rule1[AST] = rule {
      (StartIf ~ Template ~ EndIf) ~> ((refText: String, children: Seq[AST]) => AST.If(refText, children))
    }

    def Block: Rule1[AST] = rule {
      Each | If | SimpleDoubleCurlies | SimpleTripleCurlies
    }

    def Template: Rule1[Seq[AST]] = rule { RawText ~ zeroOrMore(Block).separatedBy(RawText) ~ RawText }

    def WholeTemplate: Rule1[Seq[AST]] = rule { Template ~ EOI }

  }

  /**
    * Parse a Handlebars template to discover what type of data would be needed to correctly populate it.
    */
  def parseHandlebars(input: String): ErrorsOr[HandlebarsTemplate] = {
    val parser = new TemplateParser(input)

    parser.Template.run() match {
      case Success(asts) =>
        buildRequiredTemplateData(asts).map(HandlebarsTemplate(input, _))
      case Failure(parseError: ParseError) =>
        Invalid(NonEmptyList.of(parseError.format(parser)))
      case Failure(other) =>
        Invalid(NonEmptyList.of(other.toString))
    }
  }

  private def buildRequiredTemplateData(asts: Seq[AST]): ErrorsOr[Map[String, RequiredTemplateData]] = {
    import AST._
    import scala.collection.mutable.{Map => MMap}

    sealed trait Node { def convert: ErrorsOr[RequiredTemplateData] }
    sealed trait Type extends Node
    case object Str extends Type { def convert: ErrorsOr[RequiredTemplateData] = Valid(string) }
    case class Obj(fields: MMap[String, Node]) extends Type {
      def convert: ErrorsOr[RequiredTemplateData] = _convert

      def _convert: ErrorsOr[obj] = {
        val validatedFields: ErrorsOr[List[RequiredTemplateData]] = fields.toMap.values.toList.traverse(_.convert)
        validatedFields.map(values => obj((fields.keys zip values).toMap))
      }
    }
    // represents the case where we're not sure yet what the type is
    case object StrOrObj extends Type {
      // if we're still not sure what it is at this point, default to string
      def convert: ErrorsOr[RequiredTemplateData] = Valid(string)
    }
    case class Opt(x: Type) extends Node {
      def convert = x match {
        case Str => Valid(optString)
        case o @ Obj(_) => o._convert.map(validObj => optObj(validObj.fields))
        case StrOrObj => Valid(optString)
      }
    }
    case class Loop(x: Type) extends Node {
      def convert = x match {
        case Str => Valid(strings)
        case o @ Obj(fields) => o._convert.map(validObj => objs(validObj.fields))
        case StrOrObj => Valid(strings)
      }
    }
    // means the template references it as both a string and an object
    case object Conflict extends Node {
      def convert =
        Invalid(NonEmptyList.of("Ambiguous field (TODO get the field name into this error message)"))
    }

    val root = MMap.empty[String, Node]

    // TODO recursive tree insertion logic
    def insert(ast: AST, map: MMap[String, Node], prefix: String*): Unit = (ast, prefix) match {
      case (Ref(k), Nil) => map.get(k) match {
        case Some(Str) => // already inserted, nothing to do
        case None | Some(StrOrObj) => map.put(k, Str)
        case _ => map.put(k, Conflict) // conflict! the same thing has 2 different types
      }
      case (Each(k, children), Nil) => map.get(k) match {
        case Some(Loop(_)) => // already inserted, nothing to do
        case None => map.put(k, Loop(StrOrObj))
        case _ => map.put(k, Conflict) // conflict! the same thing has 2 different types
      }
      case (If(k, children), Nil) => map.get(k) match {
        case Some(Opt(_)) => // already inserted, nothing to do
        case None => map.put(k, Opt(StrOrObj))
        case _ => map.put(k, Conflict) // conflict! the same thing has 2 different types
      }
    }

    asts.foreach { ast => insert(ast, root) }


    Obj(root)._convert.map(validObj => validObj.fields)

//    Valid(
//      asts.map {
//        case Ref(k) => (k, string)
//        case Each(k, children) => (k, strings)
//        case If(k, children) => (k, optString)
//      }.toMap
//    )
  }
}
