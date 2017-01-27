package com.ovoenergy.comms.templates.model

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAllNoShrink}
import org.scalacheck.Shapeless._
import com.ovoenergy.comms.templates.model.RequiredTemplateData._

object RequiredTemplateDataProps extends Properties("RequiredTemplateData") {

  /*
  Commented out for now because most pairs of trees are invalid and thus discarded.
  TODO: write a clever generator of valid pairs of trees
   */
//  property("combines valid trees") =
//    forAllNoShrink { (x: RequiredTemplateData, y: RequiredTemplateData) =>
//      val resultOrErrors = RequiredTemplateData.combine(x, y)
//      resultOrErrors.isValid ==> {
//        val result = resultOrErrors.getOrElse(null)
//        println(s"${result.hashCode()} ${isContainedIn(x, result)} ${isContainedIn(y, result)}")
//        isContainedIn(x, result)
//        isContainedIn(y, result)
//      }
//    }

  implicit val arbitraryKey: Arbitrary[String] = Arbitrary(Gen.oneOf(Seq("A", "B")))

  property("does not combine conflicting trees") =
    forAllNoShrink { (x: RequiredTemplateData.obj, y: RequiredTemplateData.obj) =>
      val result = RequiredTemplateData.combine(List(x, y))
      result.isInvalid ==> isConflicting(x, y)
    }

  // checks that a is entirely contained in b
  private def isContainedIn(a: RequiredTemplateData, b: RequiredTemplateData): Boolean = (a, b) match {
    case (`string`, `string`)       => true
    case (`optString`, `optString`) => true
    case (`strings`, `strings`)     => true
    case (obj(xs), obj(ys))         => xs.forall { case (k, v1) => ys.get(k).fold(false)(v2 => isContainedIn(v1, v2)) }
    case (optObj(xs), optObj(ys))   => xs.forall { case (k, v1) => ys.get(k).fold(false)(v2 => isContainedIn(v1, v2)) }
    case (objs(xs), objs(ys))       => xs.forall { case (k, v1) => ys.get(k).fold(false)(v2 => isContainedIn(v1, v2)) }
    case _ => false // types don't match
  }

  private def isConflicting(a: RequiredTemplateData, b: RequiredTemplateData): Boolean = (a, b) match {
    case (`string`, `string`)       => false
    case (`optString`, `optString`) => false
    case (`strings`, `strings`)     => false
    case (obj(xs), obj(ys))         => (xs.keySet intersect ys.keySet).exists(k => isConflicting(xs(k), ys(k)))
    case (optObj(xs), optObj(ys))   => (xs.keySet intersect ys.keySet).exists(k => isConflicting(xs(k), ys(k)))
    case (objs(xs), objs(ys))       => (xs.keySet intersect ys.keySet).exists(k => isConflicting(xs(k), ys(k)))
    case _ => true // types don't match
  }

}
