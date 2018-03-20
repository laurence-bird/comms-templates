package com.ovoenergy.comms.templates.parsing.handlebars

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.ovoenergy.comms.model._
import com.ovoenergy.comms.templates.ErrorsOr
import com.ovoenergy.comms.templates.model.RequiredTemplateData
import com.ovoenergy.comms.templates.model.RequiredTemplateData.{Fields, obj}
import com.ovoenergy.comms.templates.model.template.files.TemplateFile
import com.ovoenergy.comms.templates.model.variables._
import com.ovoenergy.comms.templates.parsing.handlebars.Validators.{Profile, ProvidedDataKey}
import shapeless.{HList, LabelledGeneric}
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.{Keys, Values}

sealed trait Validator {
  def validate(requiredData: RequiredTemplateData.obj): ErrorsOr[Unit]
  val key: ProvidedDataKey
}

object Validators {

  sealed trait ProvidedDataKey

  case object System    extends ProvidedDataKey
  case object Profile   extends ProvidedDataKey
  case object Recipient extends ProvidedDataKey

  private val providedDataKeys = Seq(System, Profile, Recipient)

  private def validateProvidedDataType[T, R <: HList, M <: HList](requiredData: RequiredTemplateData.obj,
                                                                  providedKey: ProvidedDataKey,
                                                                  clazz: Class[T])(
      implicit labelledGen: LabelledGeneric.Aux[T, R],
      keysR: Keys.Aux[R, M],
      trav: ToTraversable.Aux[M, List, Symbol]): ErrorsOr[Unit] = {

    val providedType = providedKey.toString.toLowerCase()
    val keys         = keysR.apply.toList.map(_.name)
    requiredData.fields.get(providedType) match {
      case Some(obj(fields)) =>
        val failures = fields.collect {
          case (fieldName, RequiredTemplateData.string) if !keys.contains(fieldName) =>
            s"$providedType.$fieldName is not a valid $providedType property field"
          case (fieldName, data) if data != RequiredTemplateData.string => s"$providedType.$fieldName is not a string"
        }.toList
        if (failures.nonEmpty) Invalid(NonEmptyList.fromListUnsafe(failures))
        else Valid(())
      case Some(otherType) => Invalid(NonEmptyList.of(s"$providedType property incorrect type"))
      case None            => Valid(())
    }
  }

  // TODO: do this with shapeless
  private def validatePrintRecipient(requiredData: RequiredTemplateData.obj) = {
    val expectedKeys: Map[String, RequiredTemplateData] = Map(
      "line1"    -> RequiredTemplateData.string,
      "line2"    -> RequiredTemplateData.optString,
      "town"     -> RequiredTemplateData.string,
      "county"   -> RequiredTemplateData.optString,
      "postcode" -> RequiredTemplateData.string,
      "country"  -> RequiredTemplateData.optString
    )

    val providedKey = Recipient.toString.toLowerCase()

    requiredData.fields.get(providedKey) match {
      case Some(obj(fields: Fields)) =>
        fields.get("address") match {
          case Some(obj(addressFields: Fields)) => {
            val failures = addressFields.collect {
              case (fieldName, RequiredTemplateData.string) if !expectedKeys.keys.toSet.contains(fieldName) =>
                s"$fieldName is not a valid print recipient property type"
              case (fieldName, data) if expectedKeys.get(fieldName).map(a => a != data).getOrElse(true) =>
                s"$data is not expected type ${expectedKeys.getOrElse(fieldName, "")}"
            }.toList

            if (failures.nonEmpty) Invalid(NonEmptyList.fromListUnsafe(failures))
            else Valid(())
          }
          case None => Valid(())
        }
      case Some(otherType) => Invalid(NonEmptyList.of(s"$providedKey property incorrect type"))
      case _               => Valid(())
    }
  }

  def validators(channel: Channel): List[Validator] = {
    List(
      new Validator {
        override val key = System
        override def validate(requiredData: obj): ErrorsOr[Unit] =
          validateProvidedDataType(requiredData, key, classOf[ExpectedSystem])
      },
      new Validator {
        override val key = Profile
        override def validate(requiredData: obj): ErrorsOr[Unit] =
          validateProvidedDataType(requiredData, key, classOf[CustomerProfile])
      },
      new Validator {
        override val key = Recipient
        override def validate(requiredData: obj): ErrorsOr[Unit] = {
          channel match {
            case Email => validateProvidedDataType(requiredData, Recipient, classOf[ExpectedEmailRecipient])
            case SMS   => validateProvidedDataType(requiredData, Recipient, classOf[ExpectedSMSRecipient])
            case Print => validatePrintRecipient(requiredData)
            case _     => Valid(())
          }
        }
      }
    )
  }

  private[handlebars] def processProvidedDataFields(
      requiredData: RequiredTemplateData.obj,
      templateFile: TemplateFile,
      keysToIgnore: Set[ProvidedDataKey]): ErrorsOr[RequiredTemplateData.obj] = {

    import cats.implicits._
    val validatorsToUse = validators(templateFile.channel).filterNot(v => keysToIgnore.contains(v.key))

    val validated = {
      if (validatorsToUse.isEmpty)
        Valid(())
      else
        validatorsToUse.foldMap(_.validate(requiredData))
    }

    // Remove any keys from the required templateData which have been configured to be ignored
    validated.map { v =>
      RequiredTemplateData.obj(requiredData.fields.filter((field: (String, RequiredTemplateData)) =>
        !validatorsToUse.map(_.key.toString.toLowerCase).contains(field._1)))
    }
  }

}
