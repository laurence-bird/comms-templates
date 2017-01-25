package com.ovoenergy.comms.templates.retriever

import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.s3.S3Client

class PartialsS3Retriever(s3client: S3Client) extends PartialsRetriever {

  def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String] = {
    val channel = referringFile.channel.toString.toLowerCase
    val commType = referringFile.commType.toString.toLowerCase
    val resource = s"$commType/fragments/$channel/${referringFile.format.extension}/$partialName.${referringFile.format.extension}"
    s3client.getUTF8TextFileContent(resource) match {
      case Some(content)  => Right(content)
      case None           => Left(s"Could not find shared partial: $partialName")
    }
  }

}
