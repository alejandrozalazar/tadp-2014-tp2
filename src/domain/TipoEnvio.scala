package domain

abstract class TipoEnvio

case object Normal extends TipoEnvio
case object Urgente extends TipoEnvio
case object NecesitaRefrigeracion extends TipoEnvio
case object Fragil extends TipoEnvio