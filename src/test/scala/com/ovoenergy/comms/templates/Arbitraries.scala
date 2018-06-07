package com.ovoenergy.comms.templates

import com.ovoenergy.comms.model.CommType
import com.ovoenergy.comms.templates.model.Brand
import com.ovoenergy.comms.templates.model.template.metadata.{TemplateId, TemplateSummary}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

trait Arbitraries extends com.ovoenergy.comms.model.Arbitraries {

  implicit lazy val arbTemplateId: Arbitrary[TemplateId] =
    Arbitrary(Gen.uuid.map(_.toString).map(TemplateId))

  implicit lazy val arbBrand: Arbitrary[Brand] =
    Arbitrary(Gen.oneOf(Brand.allBrands))

  implicit lazy val arbTemplateSummary: Arbitrary[TemplateSummary] =
    Arbitrary(for {
      id            <- arbitrary[TemplateId]
      commName      <- genNonEmptyString
      commType      <- arbitrary[CommType]
      brand         <- arbitrary[Brand]
      latestVersion <- genNonEmptyString
    } yield TemplateSummary(id, commName, commType, brand, latestVersion))

}
