import java.util.GregorianCalendar
import scala.collection.mutable.HashSet

package object domain {

  // presten atención al código repetido, el "filtro" en si está siendo la función 
  //   (el predicate / predicado) que recibe un elemento y retorna un boolean
  // Detalle de estilo: generalmente los tipos se definen con mayúsculas (en camel case)
  type filtro = HashSet[Viaje] => HashSet[Viaje]

  def FiltroSucursal(sucursal: Sucursal)(viajes: HashSet[Viaje]) = {
    // detalle de scala: el "==" es un equals
    viajes.filter(_.sucursalOrigen == sucursal)
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