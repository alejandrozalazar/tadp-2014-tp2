package domain

import unidadmedida.Kilometro
import unidadmedida.UnidadesFactory

class CalculadorDistancia {
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
    100.kilometros
  }
  def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
    1001.kilometros
  }
  def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
    2
  }
}