package com.ovoenergy.comms.templates.parsing

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.map._
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

    sealed trait PotentialObj extends Type

    sealed trait ConcreteType extends Type

    case class Str(markAsConflict: () => Unit) extends ConcreteType {
      def convert: ErrorsOr[RequiredTemplateData] = Valid(string)
    }

    object Obj {
      def fresh(markAsConflict: () => Unit): Obj = new Obj(MMap.empty, markAsConflict)
    }
    case class Obj(fields: MMap[String, Node], markAsConflict: () => Unit) extends PotentialObj with ConcreteType {

      def get(key: String): Option[Node] = fields.get(key)

      def put(key: String, value: Node): Option[Node] = fields.put(key, value)

      def convert: ErrorsOr[RequiredTemplateData] = _convert

      def _convert: ErrorsOr[obj] = {
        val validatedFields: ErrorsOr[Map[String, RequiredTemplateData]] = fields.toMap.traverseU(_.convert)
        validatedFields.map(fields => obj(fields))
      }

    }

    // represents the case where we're not sure yet what the type is
    case class StrOrObj(replaceWith: (Type) => Unit) extends PotentialObj {
      // if we're still not sure what it is at this point, default to string
      def convert: ErrorsOr[RequiredTemplateData] = Valid(string)
    }

    object Opt {
      def strOrObj: Opt = {
        val opt = Opt(null)
        val strOrObj = StrOrObj(replaceWith = opt.x = _)
        opt.x = strOrObj
        opt
      }
    }
    case class Opt(var x: Type) extends Node {
      def convert = x match {
        case Str(_) => Valid(optString)
        case o @ Obj(_, _) => o._convert.map(validObj => optObj(validObj.fields))
        case StrOrObj(_) => Valid(optString)
        case Conflict => Conflict.convert
      }
    }

    object Loop {
      def strOrObj: Loop = {
        val loop = Loop(null)
        val strOrObj = StrOrObj(replaceWith = loop.x = _)
        loop.x = strOrObj
        loop
      }
    }
    case class Loop(var x: Type) extends Node {
      def convert = x match {
        case Str(_) => Valid(strings)
        case o :Obj => o._convert.map(validObj => objs(validObj.fields))
        case StrOrObj(_) => Valid(strings)
        case Conflict => Conflict.convert
      }
    }

    // means the template references it as both a string and an object, or as both an option and a seq, etc.
    case object Conflict extends Type {
      def convert =
        Invalid(NonEmptyList.of("Ambiguous field (TODO get the field name into this error message)"))
    }

    val root = Obj.fresh(markAsConflict = () => ())

    object DottedKey {
      def parse(key: String): DottedKey = {
        val parts = key.split('.')
        val result = DottedKey(parts.dropRight(1), parts.last)
        result
      }
    }
    case class DottedKey(init: Seq[String], last: String)

    def insertObj(name: String, parent: Type): Obj = {
      val parentObj: Obj = parent match {
        case obj: Obj => obj
        case StrOrObj(replaceWith) =>
          // since we're trying to insert an object as a child, the parent must be an object so we upgrade it
          val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
          replaceWith(freshObj)
          freshObj
        case Str(markAsConflict) =>
          // We're trying to add a child object to a string. Mark as a conflict and return a dummy object so we can continue
          markAsConflict()
          Obj.fresh(markAsConflict)
        case Conflict =>
          Obj.fresh(markAsConflict = () => ())
      }
      parentObj.get(name) match {
        case None =>
          val obj = Obj.fresh(markAsConflict = () => parentObj.put(name, Conflict))
          parentObj.put(name, obj)
          obj
        case Some(existingObj: Obj) =>
          // nothing to do
          existingObj
        case Some(StrOrObj(replaceWith)) =>
          // upgrade to an object
          val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
          replaceWith(freshObj)
          freshObj
        case Some(Str(_)) | Some(Loop(_)) | Some(Opt(Str(_))) | Some(Opt(Conflict)) =>
          // replace with a Conflict and return a dummy Obj so we can continue
          parentObj.put(name, Conflict)
          Obj.fresh(() => ())
        case Some(Opt(existingObj: Obj)) =>
          // nothing to do
          existingObj
        case Some(Opt(StrOrObj(replaceWith))) =>
          // upgrade to an object
          val freshObj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
          replaceWith(freshObj)
          freshObj
        case Some(Conflict) =>
          // nothing to do, just return a dummy Obj
          Obj.fresh(() => ())
      }
    }

    def walkTree(rawKey: String, localRoot: Type): (DottedKey, Type) = {
      val key = DottedKey.parse(rawKey)
      val (normalisedKey, correctRoot) = chooseRoot(key, localRoot)
      val parentNode = normalisedKey.init.foldLeft(correctRoot){
        case (parent, name) => insertObj(name, parent)
      }
      (key, parentNode)
    }

    def chooseRoot(key: DottedKey, localRoot: Type): (DottedKey, Type) = {
      if (key.init.headOption.contains("this"))
        (DottedKey(key.init.tail, key.last), localRoot)
      else
        (key, root)
    }

    def insert(ast: AST, localRoot: Type): Unit = ast match {
      case Ref(k) =>
        val (key, parentNode) = walkTree(k, localRoot)
        parentNode match {
          case Str(markAsConflict) =>
            if (key.last == "this") {
              // OK, nothing to do
            } else
            // we're trying to reference a child field of a string - conflict!
              markAsConflict()
          case StrOrObj(replaceWith) =>
            if (key.last == "this") {
              // it must be a string
              val str = Str(markAsConflict = () => replaceWith(Conflict))
              replaceWith(str)
            } else {
              // it must be an object
              val obj = Obj.fresh(markAsConflict = () => replaceWith(Conflict))
              obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict)))
              replaceWith(obj)
            }
          case obj@Obj(_, markAsConflict) =>
            if (key.last == "this") {
              println(s"Conflict! $key") // TODO investigate this
              markAsConflict()
            } else {
              obj.get(key.last) match {
                case Some(Str(_)) => // already inserted, nothing to do
                case None | Some(StrOrObj(_)) => obj.put(key.last, Str(markAsConflict = () => obj.put(key.last, Conflict)))
                case Some(Opt(StrOrObj(replaceWith))) => replaceWith(Str(markAsConflict = () => replaceWith(Conflict)))
                case _ =>
                  obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
              }
            }
          case Conflict => // nothing to do
        }
      case Each(k, children) =>
        val (key, parentNode) = walkTree(k, localRoot)
        parentNode match {
          case obj: Obj =>
            obj.get(key.last) match {
              case Some(Loop(elem)) =>
                children.foreach(insert(_, localRoot = elem))
              case None =>
                val loop = Loop.strOrObj
                obj.put(key.last, loop)
                children.foreach(insert(_, localRoot = loop.x))
              case _ =>
                obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
            }
          case _ => // TODO
        }
      case If(k, children) =>
        val (key, parentNode) = walkTree(k, localRoot)
        parentNode match {
          case obj: Obj =>
            obj.get(key.last) match {
              case Some(Opt(elem)) =>
                children.foreach(insert(_, localRoot = localRoot))
              case None =>
                val opt = Opt.strOrObj
                obj.put(key.last, opt)
                children.foreach(insert(_, localRoot = localRoot))
              case _ =>
                obj.put(key.last, Conflict) // conflict! the same thing has 2 different types
            }
          case _ => // TODO
        }
    }

    asts.foreach { ast => insert(ast, root) }

    root._convert.map(validObj => validObj.fields)
  }
}
