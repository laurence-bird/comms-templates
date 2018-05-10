package com.ovoenergy.comms.templates.util

import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.{Arbitrary, Gen, Properties}

object HashSpec extends Properties("Hash") {

  implicit val arbInt = Arbitrary(Gen.posNum[Int])

  /** Taken from scalacheck, due to old transitive dependency from scalacheck-shapeless*/
  implicit val nonEmptyArbAsciiString = Arbitrary(Gen.alphaNumStr)

  property("is referentially transparent for any string") = forAllNoShrink { (str: String, int: Int) =>
    val hashValues = (1 to int).foldLeft(List.empty[String]) { (acc, elem) =>
      acc :+ Hash(str)
    }

    hashValues.distinct.length == 1
  }
}
