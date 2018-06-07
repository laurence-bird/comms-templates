package com.ovoenergy.comms.templates

import java.util.UUID

import cats.scalatest.ValidatedValues
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.services.dynamodbv2.model.{AttributeDefinition, KeySchemaElement, KeyType, ProvisionedThroughput}
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBAsyncClientBuilder, AmazonDynamoDBClientBuilder}
import com.gu.scanamo.{Scanamo, Table => ScanamoTable}
import com.ovoenergy.comms.model.Service
import com.ovoenergy.comms.templates.model.Brand.Ovo
import com.ovoenergy.comms.templates.model.template.metadata.{TemplateId, TemplateSummary}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, OptionValues}

class TemplateMetadataRepoSpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with ValidatedValues
    with BeforeAndAfterAll
    with PropertyChecks
    with Arbitraries
    with TemplateMetadataDynamoFormats {

  private val awsCreds = new AWSStaticCredentialsProvider(new BasicAWSCredentials("key", "secret"))
  private val dynamoDb = AmazonDynamoDBAsyncClientBuilder
    .standard()
    .withCredentials(awsCreds)
    .withEndpointConfiguration(new EndpointConfiguration("http://localhost:8000", "eu-west-1"))
    .build()
  private val context              = TemplateMetadataContext(dynamoDb, "templateSummaryTable")
  private val templateSummaryTable = ScanamoTable[TemplateSummary](context.tableName)

  override def beforeAll(): Unit = {
    import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
    import scala.collection.JavaConverters._
    dynamoDb.createTable(
      List(new AttributeDefinition("templateId", S)).asJava,
      context.tableName,
      List(new KeySchemaElement("templateId", KeyType.HASH)).asJava,
      new ProvisionedThroughput(1L, 1L)
    )
  }

  override def afterAll(): Unit = {
    dynamoDb.deleteTable(context.tableName)
  }

  it should "Retrieve template summary records" in forAll { templateSummary: TemplateSummary =>
    Scanamo.exec(dynamoDb)(templateSummaryTable.put(templateSummary))

    TemplateMetadataRepo.getTemplateSummary(context, templateSummary.templateId).value.value shouldBe templateSummary
  }

}
