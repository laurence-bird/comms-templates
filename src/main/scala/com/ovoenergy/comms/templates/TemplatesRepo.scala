package com.ovoenergy.comms.templates

import cats.data.{ReaderT, Validated}
import com.ovoenergy.comms.model.CommManifest
import com.ovoenergy.comms.templates.model.CommTemplate

object TemplatesRepo {

  type ErrorsOr[A] = Validated[String, A]

  def getTemplate(commManifest: CommManifest) = ReaderT[Option, TemplatesContext, CommTemplate[ErrorsOr]] { context =>
    // TODO download files from S3
    // TODO parse each template
    None
  }

}
