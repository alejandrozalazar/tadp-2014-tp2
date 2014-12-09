package domain

import unidadmedida.VolumenM3

// esta clase no tiene sentido, no debería existir
class Cliente {

  def enviarPaquete(sucursalOrigen: Sucursal, sucursalDestino: Sucursal, volumen: VolumenM3, tipoEnvio: TipoEnvio): Unit = {
    throw new Exception("Not implemented yet")
  }
}