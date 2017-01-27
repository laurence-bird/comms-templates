package com.ovoenergy.comms.templates.model.template.processed

import cats._
import cats.data.Validated.Valid
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, optString, string, strings}
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import org.scalatest.{FlatSpec, Matchers}

class CommTemplateSpec extends FlatSpec
  with Matchers
  with ValidatedMatchers {

  val reqData1 = obj(Map(
    "a" -> string,
    "b" -> optString,
    "c" -> obj(Map(
      "x" -> string))))

  val reqData2 = obj(Map(
    "b" -> optString,
    "c" -> obj(Map(
      "y" -> string)),
    "d" -> strings))

  val reqData3 = obj(Map(
    "b" -> optString,
    "c" -> obj(Map(
      "z" -> string)),
    "d" -> strings))

  it should "combine the required data from all channels forming the template" in {
    val template = CommTemplate[Id](Some(EmailTemplate[Id](
      subject = HandlebarsTemplate("", Valid(reqData1)),
      htmlBody = HandlebarsTemplate("", Valid(reqData2)),
      textBody = Some(HandlebarsTemplate("", Valid(reqData3))),
      sender = None
    )))

    template.combineRequiredData should beValid(obj(Map(
      "a" -> string,
      "b" -> optString,
      "c" -> obj(Map(
        "x" -> string,
        "y" -> string,
        "z" -> string)),
      "d" -> strings)))
  }

  it should "handle no channels when combining required data" in {
    val template = CommTemplate[Id](None)
    template.combineRequiredData should haveInvalid("No templates to combine")
  }
}
