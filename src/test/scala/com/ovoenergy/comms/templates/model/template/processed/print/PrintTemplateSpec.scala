package com.ovoenergy.comms.templates.model.template.processed.print

import cats.Id
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.RequiredTemplateData.obj
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import org.scalatest.{FlatSpec, Matchers}
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, optString, string, strings}
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.templates.ErrorsOr

class PrintTemplateSpec extends FlatSpec with Matchers with ValidatedMatchers {

  val reqData1 = obj(Map("a" -> string, "b" -> optString, "c" -> obj(Map("x" -> string))))

  val reqData2 = obj(Map("b" -> optString, "c" -> obj(Map("y" -> string)), "d" -> strings))

  val reqData3 = obj(Map("b" -> optString, "c" -> obj(Map("z" -> string)), "d" -> strings))

  it should "combine the required data from parts forming the template" in {
    val template = PrintTemplate[Id](
      header = Some(HandlebarsTemplate("", reqData1)),
      body = HandlebarsTemplate("", reqData2),
      footer = Some(HandlebarsTemplate("", reqData3))
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
          "b" -> optString,
          "c" -> obj(Map("x" -> string, "y" -> string, "z" -> string)),
          "d" -> strings)))
  }

  it should "combine the required data from parts forming the template no Text Body" in {
    val template = PrintTemplate[Id](
      header = None,
      body = HandlebarsTemplate("", reqData2),
      footer = None
    )

    template.requiredData should beValid(obj(Map("b" -> optString, "c" -> obj(Map("y" -> string)), "d" -> strings)))
  }

  it should "Combine error messages from invalid templates" in {
    val template = PrintTemplate[ErrorsOr](
      header = Some(Invalid(NonEmptyList[String]("Invalid header template", Nil))),
      body = Invalid(NonEmptyList[String]("Invalid body", Nil)),
      footer = None
    )

    template.requiredData should beInvalid(
      NonEmptyList.fromList(List("Invalid body", "Invalid header template")).get
    )
  }

  it should "return appropriate error message, if both valid and invalid templates are returned" in {
    val template = PrintTemplate[ErrorsOr](
      header = Some(Invalid(NonEmptyList[String]("Invalid header template", Nil))),
      body = Valid(HandlebarsTemplate("", reqData2)),
      footer = None
    )

    template.requiredData should beInvalid(
      NonEmptyList.fromList(List("Invalid header template")).get
    )
  }
}
