package com.ovoenergy.comms.templates

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import com.gu.scanamo.DynamoFormat
import com.ovoenergy.comms.model.CommType
import com.ovoenergy.comms.templates.model.Brand
import com.ovoenergy.comms.templates.model.template.metadata.{TemplateId, TemplateSummary}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class TemplateMetadataDynamoFormatsSpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with PropertyChecks
    with Arbitraries {

  import TemplateMetadataDynamoFormats._

  "TemplateIdFormat" should "serialise to S" in forAll { templateId: TemplateId =>
    val av = DynamoFormat[TemplateId].write(templateId)

    av.getS shouldBe templateId.value
  }

  "TemplateIdFormat" should "deserialise from S" in forAll { str: String =>
    val av = new AttributeValue().withS(str)

    DynamoFormat[TemplateId].read(av) shouldBe Right(TemplateId(str))
  }

  "CommTypeFormat" should "serialise to S" in forAll { commType: CommType =>
    val av = DynamoFormat[CommType].write(commType)

    av.getS shouldBe commType.toString
  }

  "CommTypeFormat" should "deserialise from S" in forAll { commType: CommType =>
    val av = new AttributeValue().withS(commType.toString)

    DynamoFormat[CommType].read(av) shouldBe Right(commType)
  }

  "BrandFormat" should "serialise to S" in forAll { brand: Brand =>
    val av = DynamoFormat[Brand].write(brand)

    av.getS shouldBe brand.value
  }

  "BrandFormat" should "deserialise from S" in forAll { brand: Brand =>
    val av = new AttributeValue().withS(brand.value)

    DynamoFormat[Brand].read(av) shouldBe Right(brand)
  }

  "TemplateSummaryFormat" should "serialise to M" in forAll { templateSummmary: TemplateSummary =>
    val av = DynamoFormat[TemplateSummary].write(templateSummmary)

    av.getM.get("templateId").getS shouldBe templateSummmary.templateId.value
    av.getM.get("commType").getS shouldBe templateSummmary.commType.toString
    av.getM.get("commName").getS shouldBe templateSummmary.commName
    av.getM.get("brand").getS shouldBe templateSummmary.brand.value
    av.getM.get("latestVersion").getS shouldBe templateSummmary.latestVersion
  }

  "TemplateSummaryFormat" should "deserialise from M" in forAll { ts: TemplateSummary =>
    val av = new AttributeValue()
      .addMEntry("templateId", new AttributeValue().withS(ts.templateId.value))
      .addMEntry("commType", new AttributeValue().withS(ts.commType.toString))
      .addMEntry("commName", new AttributeValue().withS(ts.commName))
      .addMEntry("brand", new AttributeValue().withS(ts.brand.value))
      .addMEntry("latestVersion", new AttributeValue().withS(ts.latestVersion))

    DynamoFormat[TemplateSummary].read(av) shouldBe Right(ts)
  }
}
