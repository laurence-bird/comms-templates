package com.ovoenergy.comms.templates.model.template.processed.print

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.instances.option._
import cats.syntax.CartesianBuilder
import cats.implicits._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import cats.{Applicative, Id}
import com.ovoenergy.comms.templates.ErrorsOr

import scala.language.higherKinds
import com.ovoenergy.comms.templates.model.{HandlebarsTemplate, RequiredTemplateData}

case class PrintTemplate[M[_]: Applicative](body: M[HandlebarsTemplate]) {

  def aggregate: M[PrintTemplate[Id]] = {
    body map (PrintTemplate[Id](_))
  }

  def requiredData: M[ErrorsOr[RequiredTemplateData.obj]] = {
    import cats.instances.list._
    body.map(b => RequiredTemplateData.combine(List(b.requiredData)))
  }
}
