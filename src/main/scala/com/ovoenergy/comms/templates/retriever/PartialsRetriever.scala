package com.ovoenergy.comms.templates.retriever

import com.ovoenergy.comms.templates.model.template.files.TemplateFile

trait PartialsRetriever {

  def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String]

}
