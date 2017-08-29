package com.ovoenergy.comms.templates.model.template.files.print

import com.ovoenergy.comms.templates.model.template.files.TemplateFile

case class PrintTemplateFiles(body: TemplateFile, header: Option[TemplateFile], footer: Option[TemplateFile])
