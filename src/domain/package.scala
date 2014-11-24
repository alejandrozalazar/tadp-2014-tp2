import java.util.GregorianCalendar
import scala.collection.mutable.HashSet

package object domain {

  type filtro = HashSet[Viaje] => HashSet[Viaje]

  def FiltroSucursal(sucursal: Sucursal)(
    viajes: HashSet[Viaje]) = {
    viajes.filter(_.sucursalOrigen.equals(sucursal))
  }

  def FiltroTransporte(transporte: Transporte)(
    viajes: HashSet[Viaje]) = {
    viajes.filter(_.transporte.equals(transporte))
  }

  def FiltroTipoEnvio(tipoEnvio: TipoEnvio)(
    viajes: HashSet[Viaje]) = {
    viajes.filter(_.envios.exists(_.tipoEnvio.equals(tipoEnvio)))
  }

  def FiltroFecha(desde: GregorianCalendar)(hasta: GregorianCalendar)(
    viajes: HashSet[Viaje]) = {
    viajes.filter { (viaje) => viaje.fechaSalida.compareTo(desde) >= 0 && viaje.fechaSalida.compareTo(hasta) <= 0 }
  }

  def FiltroOrigenDestino(origen: Sucursal)(destino: Sucursal)(
    viajes: HashSet[Viaje]) = {
    viajes.filter { (viaje) => viaje.sucursalOrigen.equals(origen) && viaje.sucursalDestino.equals(destino) }
  }
}