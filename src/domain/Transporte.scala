package domain

import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.Kilometro
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory

abstract class Transporte(val tiposDeEnviosSoportados: List[TipoEnvio], val tieneGps: Boolean, val tieneSeguimientoSatelital: Boolean) {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  val capacidad = 0.m3
  val velocidad = 0.kmh
  val costoPorKilometro = 0.pesos

  def enviosSoportados = tiposDeEnviosSoportados

}

case class Avion(override val tiposDeEnviosSoportados: List[TipoEnvio], override val tieneGps: Boolean, override val tieneSeguimientoSatelital: Boolean)
  extends Transporte(tiposDeEnviosSoportados, tieneGps, tieneSeguimientoSatelital) {
  
  override val capacidad = 200.m3
  override val velocidad = 500.kmh
  override val costoPorKilometro = 500.pesos
}

case class Camion(override val tiposDeEnviosSoportados: List[TipoEnvio], override val tieneGps: Boolean, override val tieneSeguimientoSatelital: Boolean)
  extends Transporte(tiposDeEnviosSoportados, tieneGps, tieneSeguimientoSatelital) {

  override val capacidad = 45.m3
  override val velocidad = 60.kmh
  override val costoPorKilometro = 100.pesos

  override val enviosSoportados: List[TipoEnvio] = tiposDeEnviosSoportados ++ List(NecesitaRefrigeracion)
}

case class Furgoneta(override val tiposDeEnviosSoportados: List[TipoEnvio], override val tieneGps: Boolean, override val tieneSeguimientoSatelital: Boolean)
  extends Transporte(tiposDeEnviosSoportados, tieneGps, tieneSeguimientoSatelital) {

  override val capacidad = 9.m3
  override val velocidad = 80.kmh
  override val costoPorKilometro = 40.pesos

}

