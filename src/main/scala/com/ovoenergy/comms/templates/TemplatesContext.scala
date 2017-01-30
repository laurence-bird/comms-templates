package com.ovoenergy.comms.templates

import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.services.s3.AmazonS3Client
import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.parsing.handlebars.HandlebarsParsing
import com.ovoenergy.comms.templates.retriever.{PartialsS3Retriever, TemplatesRetriever, email}
import com.ovoenergy.comms.templates.s3.AmazonS3ClientWrapper

object TemplatesContext {
  def nonCachingContext(credentialsProvider: AWSCredentialsProvider): TemplatesContext = {
    val s3Client = new AmazonS3ClientWrapper(new AmazonS3Client(credentialsProvider))
    TemplatesContext(
      emailTemplateRetriever = new email.TemplatesS3Retriever(s3Client),
      parser = new HandlebarsParsing(new PartialsS3Retriever(s3Client))
    )
  }
}

case class TemplatesContext(
                             emailTemplateRetriever: TemplatesRetriever[EmailTemplateFiles],
                             parser: Parsing[HandlebarsTemplate]
                           )

