package domain

import unidadmedida.VolumenM3
import unidadmedida.VolumenM3

class Envio(val sucursalOrigen: Sucursal, val sucursalDestino: Sucursal, val volumen: VolumenM3, val tipoEnvio: TipoEnvio = Normal) {

}