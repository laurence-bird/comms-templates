package com.ovoenergy.comms.templates.repo.email

import com.ovoenergy.comms.templates.model.TemplateFile
import com.ovoenergy.comms.templates.parsing.PartialsRepo
import com.ovoenergy.comms.templates.s3.S3Client

class Partials(s3client: S3Client) extends PartialsRepo {

  def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String] = {
    val resource = s"${referringFile.commType.toString.toLowerCase}/fragments/email/${referringFile.format.extension}/$partialName.${referringFile.format.extension}"
    s3client.getUTF8TextFileContent(resource) match {
      case Some(content)  => Right(content)
      case None           => Left(s"Could not find shared partial: $partialName")
    }
  }

}
