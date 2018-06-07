package com.ovoenergy.comms.templates.model

/**
  * @param rawExpandedContent The raw content of the Handlebars template,
  *                           with all partials recursively resolved and expanded.
  * @param requiredData       The type of data that is required to correctly populate the template.
  */
case class HandlebarsTemplate(
    rawExpandedContent: String,
    requiredData: RequiredTemplateData.obj
)
