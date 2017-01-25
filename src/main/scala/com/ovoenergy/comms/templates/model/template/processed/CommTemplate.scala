package com.ovoenergy.comms.templates.model.template.processed

import cats.instances.option._
import cats.syntax.traverse._
import cats.{Applicative, Apply, Id}
import com.ovoenergy.comms.templates.model.template.processed.email.EmailTemplate

import scala.language.higherKinds

/* A full template for a comm, which will include templates for one or more channels */
case class CommTemplate[M[_]: Applicative](
                               email: Option[M[EmailTemplate[Id]]]
                             ) {

  def aggregate: M[CommTemplate[Id]] = {
    Apply[M].map(email.sequenceU)(e => CommTemplate[Id](e))
  }

}

