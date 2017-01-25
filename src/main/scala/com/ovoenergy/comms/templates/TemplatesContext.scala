package com.ovoenergy.comms.templates

import com.ovoenergy.comms.templates.model.HandlebarsTemplate
import com.ovoenergy.comms.templates.model.template.files.email.EmailTemplateFiles
import com.ovoenergy.comms.templates.parsing.Parsing
import com.ovoenergy.comms.templates.retriever.TemplatesRetriever

case class TemplatesContext(
                             emailTemplateRetriever: TemplatesRetriever[EmailTemplateFiles],
                             parser: Parsing[HandlebarsTemplate]
                           )

