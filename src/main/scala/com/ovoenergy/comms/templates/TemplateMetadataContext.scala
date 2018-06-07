package com.ovoenergy.comms.templates

import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBAsync, AmazonDynamoDBAsyncClient}

object TemplateMetadataContext {

  def nonCaching(credentialsProvider: AWSCredentialsProvider, tableName: String): TemplateMetadataContext = {
    val amazonDynamoDBAsync = AmazonDynamoDBAsyncClient
      .asyncBuilder()
      .withCredentials(credentialsProvider)
      .build()

    TemplateMetadataContext(
      amazonDynamoDBAsync,
      tableName
    )
  }
}

case class TemplateMetadataContext private (dynamoDb: AmazonDynamoDBAsync, tableName: String) extends AutoCloseable {
  override def close(): Unit = dynamoDb.shutdown()
}
