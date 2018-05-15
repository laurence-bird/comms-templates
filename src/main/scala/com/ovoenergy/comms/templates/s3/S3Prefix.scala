package com.ovoenergy.comms.templates.s3

import com.ovoenergy.comms.model.TemplateManifest

object S3Prefix {

  def fromTemplateManifest(templateManifest: TemplateManifest) = {
    s"${templateManifest.id}/${templateManifest.version}"
  }
}
