package domain

import unidadmedida.VolumenM3
import unidadmedida.VolumenM3

class Sucursal {

  var volumenDepositoSucursal: VolumenM3 = new VolumenM3(0)
  var transportes: Set[Transporte] = Set()
  var enviosAcumulados: Set[Envio] = Set()
  var enviosLlegandoASucursal: Set[Envio] = Set()
}