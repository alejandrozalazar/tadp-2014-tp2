package domain

import java.util.Date

abstract class Filtro {

}
// Que el comportamiento esté en el uso habla del acoplamiento y la separación de responsabilidades.
// Lo que está pasando con estos objetos es que solo sirven de marcadores.
case class FiltroSucursal(val sucursal: Sucursal) extends Filtro
case class FiltroTransporte(val transporte: Transporte) extends Filtro
case class FiltroEnvio(val tipoEnvio: TipoEnvio) extends Filtro
case class FiltroFecha(val fechaDesde: Date, val fechaHasta: Date) extends Filtro