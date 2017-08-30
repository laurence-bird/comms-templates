package com.ovoenergy.comms.templates.model.template.processed

import cats._
import cats.data.Validated.Valid
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, optString, string, strings}
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate
import com.ovoenergy.comms.templates.model.template.processed.print.PrintTemplate
import com.ovoenergy.comms.templates.model.template.processed.sms.SMSTemplate
import org.scalatest.{FlatSpec, Matchers}

class CommTemplateSpec extends FlatSpec with Matchers with ValidatedMatchers {

  val reqData1 = obj(Map("a" -> string, "b" -> optString, "c" -> obj(Map("w" -> string))))

  val reqData2 = obj(Map("b" -> optString, "c" -> obj(Map("x" -> string)), "d" -> strings))

  val reqData3 = obj(Map("b" -> optString, "c" -> obj(Map("y" -> string)), "d" -> strings))

  val reqData4 = obj(Map("b" -> optString, "c" -> obj(Map("z" -> string)), "d" -> strings))

  val reqData5 = obj(Map("b" -> optString, "c" -> obj(Map("p" -> string)), "d" -> strings))

  val email = Some(
    EmailTemplate[Id](
      subject = HandlebarsTemplate("", reqData1),
      htmlBody = HandlebarsTemplate("", reqData2),
      textBody = Some(HandlebarsTemplate("", reqData3)),
      sender = None
    ))

  val sms = Some(
    SMSTemplate[Id](
      textBody = HandlebarsTemplate("", reqData4)
    ))

  val print = Some(
    PrintTemplate[Id](
      body = HandlebarsTemplate("", reqData5),
      header = Some(HandlebarsTemplate("", reqData2)),
      footer = Some(HandlebarsTemplate("", reqData3))
    )
  )

  it should "combine the required data from all channels forming the template" in {
    val template = CommTemplate[Id](
      email,
      sms,
      print
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
            "b" -> optString,
            "c" -> obj(Map("w" -> string, "x" -> string, "y" -> string, "z" -> string, "p" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from email and sms channels forming the template" in {
    val template = CommTemplate[Id](
      email,
      sms,
      None
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
            "b" -> optString,
            "c" -> obj(Map("w" -> string, "x" -> string, "y" -> string, "z" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from email and print channels forming the template" in {
    val template = CommTemplate[Id](
      email,
      None,
      print
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
            "b" -> optString,
            "c" -> obj(Map("w" -> string, "x" -> string, "y" -> string, "p" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from sms and print channels forming the template" in {
    val template = CommTemplate[Id](
      None,
      sms,
      print
    )

    template.requiredData should beValid(
      obj(
        Map("b" -> optString,
            "c" -> obj(Map("p" -> string, "x" -> string, "y" -> string, "z" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from email forming the template" in {
    val template = CommTemplate[Id](
      email,
      None,
      None
    )

    template.requiredData should beValid(
      obj(
        Map("a" -> string,
            "b" -> optString,
            "c" -> obj(Map("w" -> string, "x" -> string, "y" -> string)),
            "d" -> strings)))
  }

  it should "combine the required data from print forming the template" in {
    val template = CommTemplate[Id](
      None,
      None,
      print
    )

    template.requiredData should beValid(
      obj(Map("b" -> optString, "c" -> obj(Map("p" -> string, "x" -> string, "y" -> string)), "d" -> strings)))
  }

  it should "combine the required data from sms forming the template" in {
    val template = CommTemplate[Id](
      None,
      sms,
      None
    )

    template.requiredData should beValid(obj(Map("b" -> optString, "c" -> obj(Map("z" -> string)), "d" -> strings)))
  }

  it should "fail to combine the required data from all channels if they conflict" in {
    val conflictingReqData = obj(Map("b" -> optString, "c" -> string, "d" -> strings))

    val template = CommTemplate[Id](
      email = Some(
        EmailTemplate[Id](
          subject = HandlebarsTemplate("", reqData1),
          htmlBody = HandlebarsTemplate("", reqData2),
          textBody = Some(HandlebarsTemplate("", reqData3)),
          sender = None
        )),
      sms = Some(
        SMSTemplate[Id](
          textBody = HandlebarsTemplate("", conflictingReqData)
        )),
      print = None
    )

    template.requiredData should not be valid
  }

  it should "handle no channels when combining required data" in {
    val template = CommTemplate[Id](None, None, None)
    template.requiredData should haveInvalid("No templates to combine")
  }
}
