package com.ovoenergy.comms.templates.parsing.handlebars

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.scalatest.ValidatedMatchers
import com.ovoenergy.comms.model.{Email, Print, SMS, Service}
import com.ovoenergy.comms.templates.model.FileFormat
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{obj, optString, string, strings}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import org.scalatest.{FlatSpec, Matchers}

class ValidatorsSpec extends FlatSpec with Matchers with ValidatedMatchers {

  val emailTemplateFile = TemplateFile(Service, Email, FileFormat.Text, "")
  val smsTemplateFile   = TemplateFile(Service, SMS, FileFormat.Text, "")
  val printTemplateFile = TemplateFile(Service, Print, FileFormat.Text, "")

  it should "reject templates that references non-existent provided System parameter" in {
    val reqData1 = obj(Map("system" -> obj(Map("date" -> string))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "system.date is not a valid system property field")
  }

  it should "reject templates that references provided System parameters with the wrong type" in {
    val reqData1 = obj(Map("system" -> obj(Map("year" -> optString))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "system.year is not a string")
  }

  it should "reject templates that references the provided System parameters as the wrong type" in {
    val reqData1 = obj(Map("system" -> string))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "system property incorrect type")
  }

  it should "reject templates that references non-existent provided Profile parameter" in {
    val reqData1 = obj(Map("profile" -> obj(Map("middleName" -> string))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "profile.middleName is not a valid profile property field")
  }

  it should "reject templates that references provided Profile parameters with the wrong type" in {
    val reqData1 = obj(Map("profile" -> obj(Map("firstName" -> optString))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "profile.firstName is not a string")
  }

  it should "reject templates that references the provided Profile parameters as the wrong type" in {
    val reqData1 = obj(Map("profile" -> string))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "profile property incorrect type")
  }

  it should "reject templates that references non-existent provided Email Recipient parameter" in {
    val reqData1 = obj(Map("recipient" -> obj(Map("emailSomething" -> string))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "recipient.emailSomething is not a valid recipient property field")
  }

  it should "reject templates that references provided Email Recipient parameters with the wrong type" in {
    val reqData1 = obj(Map("recipient" -> obj(Map("emailAddress" -> optString))))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "recipient.emailAddress is not a string")
  }

  it should "reject templates that references the provided Email Recipient parameters as the wrong type" in {
    val reqData1 = obj(Map("recipient" -> string))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) should haveInvalid(
      "recipient property incorrect type")
  }

  it should "reject templates that references non-existent provided SMS Recipient parameter" in {
    val reqData1 = obj(Map("recipient" -> obj(Map("mobileNumber" -> string))))

    Validators.processProvidedDataFields(reqData1, smsTemplateFile, Set.empty) should haveInvalid(
      "recipient.mobileNumber is not a valid recipient property field")
  }

  it should "reject templates that references provided SMS Recipient parameters with the wrong type" in {
    val reqData1 = obj(Map("recipient" -> obj(Map("telephoneNumber" -> optString))))

    Validators.processProvidedDataFields(reqData1, smsTemplateFile, Set.empty) should haveInvalid(
      "recipient.telephoneNumber is not a string")
  }

  it should "reject templates that references the provided SMS Recipient parameters as the wrong type" in {
    val reqData1 = obj(Map("recipient" -> string))

    Validators.processProvidedDataFields(reqData1, smsTemplateFile, Set.empty) should haveInvalid(
      "recipient property incorrect type")
  }

  it should "accept Email templates that correctly reference provided parameters" in {
    val reqData1 = obj(
      Map(
        "field1"    -> string,
        "system"    -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(Map("emailAddress" -> string)),
        "profile"   -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    val result = obj(Map("field1" -> string))

    Validators.processProvidedDataFields(reqData1, emailTemplateFile, Set.empty) shouldBe Valid(result)
  }

  it should "accept SMS templates that correctly reference provided parameters" in {
    val reqData1 = obj(
      Map(
        "field1"    -> string,
        "system"    -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(Map("telephoneNumber" -> string)),
        "profile"   -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    val result = obj(Map("field1" -> string))

    Validators.processProvidedDataFields(reqData1, smsTemplateFile, Set.empty) shouldBe Valid(result)
  }

  it should "accept Print templates that correctly reference provided parameters" in {
    val reqData1 = obj(
      Map(
        "field1" -> string,
        "system" -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(Map(
          "address" -> obj(Map("line1" -> string, "town" -> string, "postcode" -> string, "county" -> optString)))),
        "profile" -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    val result = obj(Map("field1" -> string))

    Validators.processProvidedDataFields(reqData1, printTemplateFile, Set.empty) shouldBe Valid(result)
  }

  it should "reject Print templates that reference valid parameter names with the wrong type" in {
    val reqData1 = obj(
      Map(
        "field1" -> string,
        "system" -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(
          Map("address" -> obj(Map("line1" -> string, "town" -> string, "postcode" -> string, "county" -> strings)))),
        "profile" -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    Validators.processProvidedDataFields(reqData1, printTemplateFile, Set.empty) shouldBe Invalid(
      NonEmptyList.of("strings is not expected type optString"))
  }

  it should "reject Print templates that reference invalid parameters" in {
    val reqData1 = obj(
      Map(
        "field1" -> string,
        "system" -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(Map("address" -> obj(
          Map("line1" -> string, "town" -> string, "postcode" -> string, "county" -> optString, "yolo" -> string)))),
        "profile" -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    Validators.processProvidedDataFields(reqData1, printTemplateFile, Set.empty) shouldBe Invalid(
      NonEmptyList.of("yolo is not a valid print recipient property type"))
  }

  it should "Include all fields in required template data if specified to do so" in {
    val reqData1 = obj(
      Map(
        "field1" -> string,
        "system" -> obj(Map("month" -> string, "dayOfMonth" -> string, "year" -> string)),
        "recipient" -> obj(Map(
          "address" -> obj(Map("line1" -> string, "town" -> string, "postcode" -> string, "county" -> optString)))),
        "profile" -> obj(Map("firstName" -> string, "lastName" -> string))
      ))

    Validators.processProvidedDataFields(
      reqData1,
      printTemplateFile,
      Set(Validators.System, Validators.Profile, Validators.Recipient)) shouldBe Valid(reqData1)
  }
}
