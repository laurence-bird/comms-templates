package com.ovoenergy.comms.templates.model.template.files.email

import com.ovoenergy.comms.templates.model.template.files.TemplateFile

case class EmailTemplateFiles(
                            subject: TemplateFile,
                            htmlBody: TemplateFile,
                            textBody: Option[TemplateFile],
                            sender: Option[String]
                            ) {



}
