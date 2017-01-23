package com.ovoenergy.comms.templates.parsing

import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.TemplateFile

trait PartialsRepo {

  def getSharedPartial(referringFile: TemplateFile, partialName: String): Either[String, String]

}
