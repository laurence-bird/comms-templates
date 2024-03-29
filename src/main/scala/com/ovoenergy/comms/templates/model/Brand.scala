package com.ovoenergy.comms.templates.model

sealed trait Brand {
  def value: String

  def displayValue: String
}

object Brand {

  case object Ovo extends Brand {
    override val value        = "ovo"
    override val displayValue = "OVO"
  }

  case object Boost extends Brand {
    override val value        = "boost"
    override val displayValue = "Boost"
  }

  case object Lumo extends Brand {
    override val value        = "lumo"
    override val displayValue = "Lumo"
  }

  case object Corgi extends Brand {
    override val value        = "corgi"
    override val displayValue = "Corgi"
  }

  case object Vnet extends Brand {
    override val value        = "vnet"
    override val displayValue = "VNet"
  }

  case object PeterboroughEnergy extends Brand {
    override val value        = "peterboroughEnergy"
    override val displayValue = "Peterborough Energy"
  }

  case object EnergySW extends Brand {
    override val value        = "energySW"
    override val displayValue = "Energy SW"
  }

  case object Fairerpower extends Brand {
    override val value        = "fairerpower"
    override val displayValue = "Fairerpower"
  }

  case object SouthendEnergy extends Brand {
    override val value        = "southendEnergy"
    override val displayValue = "Southend Energy"
  }

  case object ChargedEv extends Brand {
    override val value        = "chargedev"
    override val displayValue = "Charged EV"
  }

  case object SparkEnergy extends Brand {
    override def value: String        = "sparkEnergy"
    override def displayValue: String = "Spark Energy"
  }

  case object OvoFrance extends Brand {
    override def value: String        = "ovofrance"
    override def displayValue: String = "OVO France"
  }

  case object OvoSpain extends Brand {
    override def value: String        = "ovospain"
    override def displayValue: String = "OVO Spain"
  }

  val allBrands: IndexedSeq[Brand] = Vector(
    Ovo,
    Boost,
    Lumo,
    Corgi,
    Vnet,
    PeterboroughEnergy,
    EnergySW,
    Fairerpower,
    SouthendEnergy,
    ChargedEv,
    SparkEnergy,
    OvoFrance,
    OvoSpain
  )

  def fromString(str: String): Option[Brand] = allBrands.find(_.value == str)

  def fromStringCaseInsensitive(str: String): Option[Brand] = allBrands.find(_.value.toLowerCase == str.toLowerCase)

  def unsafeFromString(str: String): Brand = fromString(str).get

  def unsafeFromStringCaseInsensitive(str: String): Brand = fromStringCaseInsensitive(str).get

}
