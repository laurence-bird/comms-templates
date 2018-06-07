package com.ovoenergy.comms.templates

import cats.data.{EitherT, NonEmptyList}
import com.gu.scanamo.error.DynamoReadError
import com.gu.scanamo.{Scanamo, Table}
import com.ovoenergy.comms.templates.model.template.metadata.{TemplateId, TemplateSummary}
import com.gu.scanamo.syntax._
import cats.instances.option._

object TemplateMetadataRepo extends TemplateMetadataDynamoFormats {

  def getTemplateSummary(context: TemplateMetadataContext, templateId: TemplateId): Option[ErrorsOr[TemplateSummary]] = {
    val table = Table[TemplateSummary](context.tableName)
    val query = table.get('templateId -> templateId.value)

    val result = Scanamo.exec(context.dynamoDb)(query)

    EitherT(result).leftMap { err: DynamoReadError =>
      NonEmptyList.of(DynamoReadError.describe(err))

    }.toValidated
  }
}
