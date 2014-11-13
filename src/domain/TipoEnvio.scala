package domain

abstract class TipoEnvio(val costo: Double)  // TODO not a double!

case object Normal extends TipoEnvio(10)
case object Urgente extends TipoEnvio(20)
case object Fragil extends TipoEnvio(18)
case object NecesitaRefrigeracion extends TipoEnvio(70)