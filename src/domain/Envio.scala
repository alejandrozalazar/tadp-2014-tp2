package domain

import unidadmedida.VolumenM3

class Envio(val sucursalOrigen: Sucursal, val sucursalDestino: Sucursal, val volumen: VolumenM3, val tipoEnvio: TipoEnvio = Normal, val naturaleza: Naturaleza = Otro) {

  // no deberían usar null teniendo Option
  // null solo lleva a null pointer exceptions o a código infinitamente defensivo 
  require(sucursalOrigen != null, "La sucursal origen es obligatoria")
  require(sucursalDestino != null, "La sucursal destino es obligatoria")
  require(!sucursalOrigen.equals(sucursalDestino), "Las sucursales origen y destino deben ser distintas")
  require(volumen > VolumenM3(0), "El volumen debe ser mayor a cero")

  def costo = {
    tipoEnvio.costo
  }

  def precio = {
    tipoEnvio.precio
  }
}