package domain

import unidadmedida.VolumenM3

case class Envio(val sucursalDestino: Sucursal, val volumen: VolumenM3, val tipoEnvio: TipoEnvio, val naturaleza: Naturaleza) {

  /*
  require(sucursalOrigen != null, "La sucursal origen es obligatoria")
  require(sucursalDestino != null, "La sucursal destino es obligatoria")
  require(!sucursalOrigen.equals(sucursalDestino), "Las sucursales origen y destino deben ser distintas")
  require(volumen > VolumenM3(0), "El volumen debe ser mayor a cero")*/
  
  def costo = {
    tipoEnvio.costo
    
  }
  
  def precio = {
    tipoEnvio.precio 
  }
}