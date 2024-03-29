package com.ovoenergy.comms.templates.parsing

import com.ovoenergy.comms.templates._
import com.ovoenergy.comms.templates.model.template.files.TemplateFile

trait Parsing[A] {

  def parseTemplate(templateFile: TemplateFile): ErrorsOr[A]

}
