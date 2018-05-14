package com.ovoenergy.comms.templates

import cats.Id
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.services.s3.AmazonS3Client
import com.ovoenergy.comms.model.{CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates.cache.CachingStrategy
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.parsing.handlebars.HandlebarsParsing
import com.ovoenergy.comms.templates.retriever.{PartialsS3Retriever, TemplatesRetriever, TemplatesS3Retriever}
import com.ovoenergy.comms.templates.s3.AmazonS3ClientWrapper

object TemplatesContext {

  /**
    * A context that provides no caching of templates.
    * Not recommended in production.
    */
  def nonCachingContext(credentialsProvider: AWSCredentialsProvider,
                        bucket: String = "ovo-comms-templates"): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider), bucket)
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client)),
      cachingStrategy = CachingStrategy.noCache[TemplateManifest, ErrorsOr[CommTemplate[Id]]]
    )
  }

  /**
    * A context that memoizes downloaded and parsed templates in a Caffeine cache.
    * The cache is bounded to 100 templates and has no expiry.
    */
  def cachingContext(credentialsProvider: AWSCredentialsProvider,
                     bucket: String = "ovo-comms-templates"): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider), bucket)
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client)),
      cachingStrategy = CachingStrategy.caffeine[TemplateManifest, ErrorsOr[CommTemplate[Id]]](maximumSize = 100)
    )
  }

  /**
    * A context that memoizes downloaded and parsed templates in a cache of your choice.
    */
  def customCachingContext(credentialsProvider: AWSCredentialsProvider,
                           cachingStrategy: CachingStrategy[TemplateManifest, ErrorsOr[CommTemplate[Id]]],
                           bucket: String = "ovo-comms-templates"): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider), bucket)
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client)),
      cachingStrategy = cachingStrategy
    )
  }
}

case class TemplatesContext(
    templatesRetriever: TemplatesRetriever,
    parser: Parsing[HandlebarsTemplate],
    cachingStrategy: CachingStrategy[TemplateManifest, ErrorsOr[CommTemplate[Id]]]
)
