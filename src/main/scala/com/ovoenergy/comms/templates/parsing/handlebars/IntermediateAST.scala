package com.ovoenergy.comms.templates.parsing.handlebars

import cats.{Traverse, UnorderedTraverse}
import cats.data.{NonEmptyList, Validated}
import cats.instances.map._
import cats.syntax.traverse._
import cats.instances.set._
import cats.data.NonEmptyList._
import cats.data.Validated.{Invalid, Valid, catsDataCommutativeApplicativeForValidated}
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.RequiredTemplateData._

import scala.collection.mutable.{Map => MMap}

private[parsing] object IntermediateAST {

  sealed trait Node {
    def toRequiredTemplateData: ErrorsOr[RequiredTemplateData]
  }

  /**
    * Represents a type, which may be a concrete type, not yet concretely known, or invalid due to conflict.
    *
    * Subtypes: Str, Obj, StrOrObj, Conflict
    */
  sealed trait Type extends Node

  /**
    * Represents something that is either an Obj or can become one.
    *
    * Subtypes: Obj, StrOrObj
    */
  sealed trait PotentialObj extends Type

  /**
    * Represents a type which is concretely known.
    *
    * Subtypes: Str, Obj
    */
  sealed trait ConcreteType extends Type

  /**
    * Represents the String type
    *
    * @param markAsConflict Function to replace this node in the tree with a Conflict node.
    */
  case class Str(markAsConflict: () => Unit) extends ConcreteType {
    def toRequiredTemplateData: ErrorsOr[RequiredTemplateData] = Valid(string)
  }

  object Obj {
    def fresh(markAsConflict: () => Unit): Obj = new Obj(MMap.empty, markAsConflict)
  }

  /**
    * Represents an object type, i.e. a Map[String, Node]
    *
    * @param markAsConflict Function to replace this node in the tree with a Conflict node.
    */
  case class Obj(fields: MMap[String, Node], markAsConflict: () => Unit) extends PotentialObj with ConcreteType {

    def get(key: String): Option[Node] = fields.get(key)

    def put(key: String, value: Node): Option[Node] = fields.put(key, value)

    def toRequiredTemplateData: ErrorsOr[RequiredTemplateData] = _convert

    def _convert: ErrorsOr[obj] = {

      type StringyMap[A] = Map[String, A]
      type ErrorsOrU[A]  = Validated[Set[String], A]
      type ErrorsOrMap   = ErrorsOrU[StringyMap[RequiredTemplateData]]
      val traverse = UnorderedTraverse[StringyMap]

      /*
       * because traversing a map is inherently unordered our errors must be also
       * and NonEmptyList does not fit the bill, the closest we can get is a Set really
       * so we shove our errors into a set then turn them unsafely back into a NEL at the end
       */
      val validatedFields: ErrorsOrMap = traverse.unorderedTraverse(fields.toMap) { node =>
        node.toRequiredTemplateData.leftMap(_.toList.toSet)
      }

      validatedFields
        .leftMap(s => NonEmptyList.fromListUnsafe(s.toList))
        .map(fields => obj(fields))
    }
  }

  /**
    * Represents the case where we're not yet sure whether it is a Str or an Obj.
    *
    * If we're still unsure after processing all the input ASTs,
    * we decide by convention that it is a Str.
    *
    * @param replaceWith Function to replace this node in the tree with the given node.
    */
  case class StrOrObj(replaceWith: (Type) => Unit) extends PotentialObj {
    def toRequiredTemplateData: ErrorsOr[RequiredTemplateData] = Valid(string)
  }

  object Opt {
    def strOrObj: Opt = {
      val opt      = Opt(null)
      val strOrObj = StrOrObj(replaceWith = opt.x = _)
      opt.x = strOrObj
      opt
    }
  }

  /**
    * Represents the fact that its child node is an optional type.
    * e.g. if the child was a `Str`, this node would represent `Option[String]`.
    */
  case class Opt(var x: Type) extends Node {
    def toRequiredTemplateData = x match {
      case _: Str      => Valid(optString)
      case o: Obj      => o._convert.map(validObj => optObj(validObj.fields))
      case _: StrOrObj => Valid(optString)
      case c: Conflict => c.toRequiredTemplateData
    }
  }

  object Loop {
    def strOrObj: Loop = {
      val loop     = Loop(null)
      val strOrObj = StrOrObj(replaceWith = loop.x = _)
      loop.x = strOrObj
      loop
    }
  }

  /**
    * Represents the fact that its child node is a list.
    * e.g. if the child was a `Str`, this node would represent a list of strings.
    */
  case class Loop(var x: Type) extends Node {
    def toRequiredTemplateData = x match {
      case _: Str      => Valid(strings)
      case o: Obj      => o._convert.map(validObj => objs(validObj.fields))
      case _: StrOrObj => Valid(strings)
      case c: Conflict => c.toRequiredTemplateData
    }
  }

  /**
    * Represents the fact that the template references the same key as different types,
    * e.g. as both a string and an object, or as both an option and a list, etc.
    */
  case class Conflict(key: String) extends Type {
    def toRequiredTemplateData =
      Invalid(NonEmptyList.of(s"Field with key '$key' has an ambiguous type."))
  }

}
