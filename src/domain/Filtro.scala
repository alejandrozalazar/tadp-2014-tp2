package domain

import java.util.Date

abstract class Filtro {

}

case class FiltroSucursal(val sucursal: Sucursal) extends Filtro
case class FiltroTransporte(val transporte: Transporte) extends Filtro
case class FiltroEnvio(val tipoEnvio: TipoEnvio) extends Filtro
case class FiltroFecha(val fechaDesde: Date, val fechaHasta: Date) extends Filtro