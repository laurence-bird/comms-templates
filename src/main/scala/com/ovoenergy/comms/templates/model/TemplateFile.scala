package com.ovoenergy.comms.templates.model

import com.ovoenergy.comms.model.{Channel, CommType}


case class TemplateFile(commType: CommType, channel: Channel, format: FileFormat, content: String)
