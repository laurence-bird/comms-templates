package com.ovoenergy.comms.templates

import cats.Id
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.services.s3.AmazonS3Client
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.cache.CachingStrategy
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.template.processed.CommTemplate
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.parsing.handlebars.{HandlebarsParsing, Validators}
import com.ovoenergy.comms.templates.retriever.{PartialsS3Retriever, TemplatesRetriever, TemplatesS3Retriever}
import com.ovoenergy.comms.templates.s3.AmazonS3ClientWrapper

object TemplatesContext {

  /**
    * A context that provides no caching of templates.
    *
    * KeysToIgnore allows one to configure whether provided data keys are validated and removed from the
    * requiredTemplateData when templates are parsed
    *
    * Not recommended in production.
    */
  def nonCachingContext(credentialsProvider: AWSCredentialsProvider,
                        keysToIgnore: Set[Validators.ProvidedDataKey] = Set.empty): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider))
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client), keysToIgnore),
      cachingStrategy = CachingStrategy.noCache[CommManifest, ErrorsOr[CommTemplate[Id]]]
    )
  }

  /**
    * A context that memoizes downloaded and parsed templates in a Caffeine cache.
    *
    * KeysToIgnore allows one to configure whether provided data keys are validated and removed from the
    * requiredTemplateData when templates are parsed
    *
    * The cache is bounded to 100 templates and has no expiry.
    */
  def cachingContext(credentialsProvider: AWSCredentialsProvider,
                     keysToIgnore: Set[Validators.ProvidedDataKey] = Set.empty): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider))
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client), keysToIgnore),
      cachingStrategy = CachingStrategy.caffeine[CommManifest, ErrorsOr[CommTemplate[Id]]](maximumSize = 100)
    )
  }

  /**
    * A context that memoizes downloaded and parsed templates in a cache of your choice.
    *
    * KeysToIgnore allows one to configure whether provided data keys are validated and removed from the
    * requiredTemplateData when templates are parsed
    *
    */
  def customCachingContext(credentialsProvider: AWSCredentialsProvider,
                           cachingStrategy: CachingStrategy[CommManifest, ErrorsOr[CommTemplate[Id]]],
                           keysToIgnore: Set[Validators.ProvidedDataKey] = Set.empty): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider))
    TemplatesContext(
      templatesRetriever = new TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client), keysToIgnore),
      cachingStrategy = cachingStrategy
    )
  }
}

case class TemplatesContext(
    templatesRetriever: TemplatesRetriever,
    parser: Parsing[HandlebarsTemplate],
    cachingStrategy: CachingStrategy[CommManifest, ErrorsOr[CommTemplate[Id]]]
)
