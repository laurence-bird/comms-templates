package com.ovoenergy.comms.templates.model.template.metadata

import com.ovoenergy.comms.model.CommType
import com.ovoenergy.comms.templates.model.Brand

case class TemplateSummary(templateId: TemplateId,
                           commName: String,
                           commType: CommType,
                           brand: Brand,
                           latestVersion: String)
