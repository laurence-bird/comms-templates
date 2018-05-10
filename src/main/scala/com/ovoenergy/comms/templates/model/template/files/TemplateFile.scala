package com.ovoenergy.comms.templates.model.template.files

import com.ovoenergy.comms.model.{Channel, CommType}
import com.ovoenergy.comms.templates.model.FileFormat

case class TemplateFile(channel: Channel, format: FileFormat, content: String)
