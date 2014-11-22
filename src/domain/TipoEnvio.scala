package domain

import unidadmedida.Dinero

abstract class TipoEnvio(val costo: Dinero, val precio: Dinero)

case object Normal extends TipoEnvio(Dinero(10), Dinero(80))
case object Urgente extends TipoEnvio(Dinero(20), Dinero(110))
case object Fragil extends TipoEnvio(Dinero(18), Dinero(120))
case object NecesitaRefrigeracion extends TipoEnvio(Dinero(70), Dinero(210))