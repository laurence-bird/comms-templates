package com.ovoenergy.comms.templates.extensions

import com.ovoenergy.comms.model.{CommManifest, TemplateManifest}
import com.ovoenergy.comms.templates.util.Hash

trait CommManifestExtensions {
  implicit class CommManifestExtensions(commManifest: CommManifest) {

    def toTemplateManifest: TemplateManifest = {
      TemplateManifest(
        Hash(commManifest.name),
        commManifest.version
      )
    }
  }
}
