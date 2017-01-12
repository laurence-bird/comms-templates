package com.ovoenergy.comms.templates.model

sealed trait RequiredTemplateData

object RequiredTemplateData {
  case object string extends RequiredTemplateData
  case object optString extends RequiredTemplateData
  case object strings extends RequiredTemplateData

  case class obj(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
  case class optObj(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
  case class objs(fields: Map[String, RequiredTemplateData]) extends RequiredTemplateData
}

