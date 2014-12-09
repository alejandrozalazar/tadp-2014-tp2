package domain

import unidadmedida.VolumenM3

// lo mismo que en master
class Cliente {

  def enviarPaquete(sucursalOrigen: Sucursal, sucursalDestino: Sucursal, volumen: VolumenM3, tipoEnvio: TipoEnvio): Unit = {
    throw new Exception("Not implemented yet")
  }
}