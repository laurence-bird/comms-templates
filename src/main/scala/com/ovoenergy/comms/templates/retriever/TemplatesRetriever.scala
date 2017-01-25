package com.ovoenergy.comms.templates.retriever

import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates._

trait TemplatesRetriever[A] {

  def getTemplate(commManifest: CommManifest): Option[ErrorsOr[A]]

}
