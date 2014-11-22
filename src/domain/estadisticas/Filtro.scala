package domain.estadisticas

import domain.Sucursal
import domain.Transporte
import domain.TipoEnvio
import domain.Viaje
import java.util.GregorianCalendar
import scala.collection.mutable.HashSet

abstract class Filtro {
	def filtrar(viajes: HashSet[Viaje]): HashSet[Viaje]
}

case class FiltroSucursal(sucursal: Sucursal) extends Filtro {
  def filtrar(viajes: HashSet[Viaje]) = {
    viajes.filter(_.sucursalOrigen.equals(sucursal))
  }
}

case class FiltroTransporte(transporte: Transporte) extends Filtro {
  def filtrar(viajes: HashSet[Viaje]) = {
    viajes.filter(_.transporte.equals(transporte))
  }
}

case class FiltroTipoEnvio(tipoEnvio: TipoEnvio) extends Filtro {
  def filtrar(viajes: HashSet[Viaje]) = {
    viajes.filter(_.envios.exists(_.tipoEnvio.equals(tipoEnvio)))
  }
}

case class FiltroFecha(desde: GregorianCalendar, hasta: GregorianCalendar) extends Filtro {
  def filtrar(viajes: HashSet[Viaje]) = {
    viajes.filter{ (viaje) => viaje.fechaSalida.compareTo(desde) >= 0 && viaje.fechaSalida.compareTo(hasta) <= 0 }
  }
}
