package com.ovoenergy.comms.templates.model

import cats.data.Validated.Valid
import com.ovoenergy.comms.templates.model.RequiredTemplateData._
import org.scalatest.{FlatSpec, Matchers}

class RequiredTemplateDataSpec extends FlatSpec with Matchers {

  behavior of "combine"

  it should "combine two valid objects into one" in {
    val fst = obj(Map(
      "a" -> string,
      "b" -> optString,
      "c" -> obj(Map(
        "x" -> string))))

    val snd = obj(Map(
      "b" -> optString,
      "c" -> obj(Map(
        "y" -> string)),
      "d" -> strings))

    RequiredTemplateData.combine(fst, snd) should be(Valid(obj(Map(
      "a" -> string,
      "b" -> optString,
      "c" -> obj(Map(
        "x" -> string,
        "y" -> string)),
      "d" -> strings))
    ))
  }

  it should "fail if two fields in an object have conflicting types" in {
    val fst = obj(Map(
      "a" -> string,
      "b" -> strings,
      "c" -> obj(Map(
        "x" -> string))))

    val snd = obj(Map(
      "b" -> optString,
      "c" -> obj(Map(
        "x" -> optString,
        "y" -> string)),
      "d" -> strings))

    val result = RequiredTemplateData.combine(fst, snd)
    result should be('Invalid)
    result.swap.foreach { nel =>
      nel.exists(_.startsWith("b is referenced")) should be (true)
      nel.exists(_.startsWith("c.x is referenced")) should be (true)
    }
  }

}
