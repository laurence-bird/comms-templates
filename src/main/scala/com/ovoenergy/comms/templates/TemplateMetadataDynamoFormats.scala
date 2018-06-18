package com.ovoenergy.comms.templates

import com.gu.scanamo.DynamoFormat
import com.gu.scanamo.error.TypeCoercionError
import com.ovoenergy.comms.model.CommType
import com.ovoenergy.comms.templates.model.Brand
import com.ovoenergy.comms.templates.model.template.metadata.{TemplateId, TemplateSummary}

trait TemplateMetadataDynamoFormats {

  implicit lazy val commTypeFormat: DynamoFormat[CommType] = DynamoFormat.xmap[CommType, String](str =>
    CommType.fromString(str).toRight(TypeCoercionError(new RuntimeException(s"$str is not a valid CommType"))))(
    _.toString)

  implicit lazy val brandFormat: DynamoFormat[Brand] = DynamoFormat.xmap[Brand, String](
    str =>
      Brand
        .fromStringCaseInsensitive(str)
        .toRight(TypeCoercionError(new RuntimeException(s"$str is not a valid Brand"))))(_.value)

  implicit lazy val templateIdFormat: DynamoFormat[TemplateId] =
    DynamoFormat.iso[TemplateId, String](TemplateId)(_.value)

}

object TemplateMetadataDynamoFormats extends TemplateMetadataDynamoFormats
