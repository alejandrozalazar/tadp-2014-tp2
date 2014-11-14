package domain

abstract class TipoEnvio(val costo: Double, val precio: Double)  // TODO not a double!

case object Normal extends TipoEnvio(10, 80)
case object Urgente extends TipoEnvio(20, 110)
case object Fragil extends TipoEnvio(18, 120)
case object NecesitaRefrigeracion extends TipoEnvio(70, 210)