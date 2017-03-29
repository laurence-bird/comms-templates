package com.ovoenergy.comms.templates.model.template.processed.email

import cats.Id
import cats.data.Validated.Valid
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, optString, string, strings}
import org.scalatest.{FlatSpec, Matchers}

class EmailTemplateSpec extends FlatSpec with Matchers with ValidatedMatchers {

  val reqData1 = obj(Map("a" -> string, "b" -> optString, "c" -> obj(Map("x" -> string))))

  val reqData2 = obj(Map("b" -> optString, "c" -> obj(Map("y" -> string)), "d" -> strings))

  val reqData3 = obj(Map("b" -> optString, "c" -> obj(Map("z" -> string)), "d" -> strings))

  behavior of "EmailTemplate"

  it should "combine the required data from parts forming the template" in {
    val template = EmailTemplate[Id](
      subject = HandlebarsTemplate("", reqData1),
      htmlBody = HandlebarsTemplate("", reqData2),
      textBody = Some(HandlebarsTemplate("", reqData3)),
      sender = None
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
            "b" -> optString,
            "c" -> obj(Map("x" -> string, "y" -> string, "z" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from parts forming the template no Text Body" in {
    val template = EmailTemplate[Id](
      subject = HandlebarsTemplate("", reqData1),
      htmlBody = HandlebarsTemplate("", reqData2),
      textBody = None,
      sender = None
    )

    template.requiredData should beValid(
      obj(Map("a" -> string, "b" -> optString, "c" -> obj(Map("x" -> string, "y" -> string)), "d" -> strings)))
  }

}
