package com.ovoenergy.comms.templates.model

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.traverse._
import cats.instances.list._
import com.ovoenergy.comms.templates.ErrorsOr

sealed trait RequiredTemplateData {
  def description: String
}

object RequiredTemplateData {
  type Fields = Map[String, RequiredTemplateData]

  case object string extends RequiredTemplateData { val description = "a mandatory string" }
  case object optString extends RequiredTemplateData { val description = "an optional string" }
  case object strings extends RequiredTemplateData { val description = "a list of strings" }

  case class obj(fields: Fields) extends RequiredTemplateData { val description = "a mandatory object" }
  case class optObj(fields: Fields) extends RequiredTemplateData { val description = "an optional object" }
  case class objs(fields: Fields) extends RequiredTemplateData { val description = "a list of objects" }

  /**
    * Combine two trees into one, returning errors for any conflicts,
    * i.e. if the same node being referenced as two different types.
    */
  def combine(requiredTemplateDatas: List[RequiredTemplateData.obj]): ErrorsOr[RequiredTemplateData.obj] = {
    requiredTemplateDatas.foldLeft[ErrorsOr[RequiredTemplateData.obj]](Valid(RequiredTemplateData.obj(Map()))){
      case (Valid(requireData), x)  => combine2(requireData, x)
      case (invalid, _)             => invalid
    }
  }

  private def combine2(fst: RequiredTemplateData.obj, snd: RequiredTemplateData.obj): ErrorsOr[RequiredTemplateData.obj] = {
    def invalid(a: RequiredTemplateData, b: RequiredTemplateData, path: Vector[String]): ErrorsOr[RequiredTemplateData] =
      Invalid(NonEmptyList.of(
        s"${path.mkString(".")} is referenced as both ${a.description} and ${b.description}}"
      ))

    def recurseFields(fields1: Fields, fields2: Fields, path: Vector[String]): ErrorsOr[Fields] = {
      val commonKeys = fields1.keySet.intersect(fields2.keySet).toList

      val aggregatedCommonValues: ErrorsOr[List[RequiredTemplateData]] =
        commonKeys.traverseU(k => run(fields1(k), fields2(k), path :+ k))

      val aggregatedCommonFields: ErrorsOr[Fields] =
        aggregatedCommonValues.map(values => (commonKeys zip values).toMap)

      aggregatedCommonFields.map(acf => fields1 ++ fields2 ++ acf)
    }

    def sameSimpleType: PartialFunction[(RequiredTemplateData, RequiredTemplateData, Vector[String]), ErrorsOr[RequiredTemplateData]] = {
      case (`string`, `string`, _) => Valid(`string`)
      case (`optString`, `optString`, _) => Valid(`optString`)
      case (`strings`, `strings`, _) => Valid(`strings`)
    }

    def bothObj: PartialFunction[(RequiredTemplateData, RequiredTemplateData, Vector[String]), ErrorsOr[RequiredTemplateData]] = {
      case (obj(fields1), obj(fields2), path) => recurseFields(fields1, fields2, path).map(obj)
    }

    def bothOptObj: PartialFunction[(RequiredTemplateData, RequiredTemplateData, Vector[String]), ErrorsOr[RequiredTemplateData]] = {
      case (optObj(fields1), optObj(fields2), path) => recurseFields(fields1, fields2, path).map(optObj)
    }

    def bothObjs: PartialFunction[(RequiredTemplateData, RequiredTemplateData, Vector[String]), ErrorsOr[RequiredTemplateData]] = {
      case (objs(fields1), objs(fields2), path) => recurseFields(fields1, fields2, path).map(objs)
    }

    def run(a: RequiredTemplateData, b: RequiredTemplateData, path: Vector[String]): ErrorsOr[RequiredTemplateData] = {
      (sameSimpleType orElse bothObj orElse bothOptObj orElse bothObjs).applyOrElse(
        (a, b, path),
        default = (_: Any) => invalid(a, b, path)
      )
    }

    recurseFields(fst.fields, snd.fields, Vector.empty).map(obj)
  }

}


