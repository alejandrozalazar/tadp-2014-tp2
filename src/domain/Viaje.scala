package domain

import java.util.Date
import unidadmedida.VolumenM3
import exceptions.ValidacionException
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory
import java.util.GregorianCalendar
import unidadmedida.Hora

case class Viaje(
  val sucursalOrigen: Sucursal,
  val sucursalDestino: Sucursal, 
  val transporte: Transporte = null, // no usar null! es obligatorio pasar un transporte
  val fechaSalida: GregorianCalendar = new GregorianCalendar(2014, 1, 1),  // no puede haber una fecha como default! 
  val envios: Set[Envio] = Set(), 
  val costoFacturado: Dinero = Dinero(0), 
  val ganancia: Dinero = Dinero(0)) {

  // detalle de scala: pueden usar "implicit class" en vez de hacer la clase y el implicit def
  //  (luego solo tienen que importar la implicit class donde la necesiten)
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  def tieneEnvios = !envios.isEmpty

  def agregarEnvio(envio: Envio): Viaje = {
    this.copy(envios = this.envios + envio)
  }

  // detalle de scala, pueden definir métodos sin paréntesis (si los invocan siempre sin paréntesis)
  def costoPaquetes(): Dinero = {
    envios.foldLeft(0.pesos) { (costoTotal, envio) =>
      costoTotal + envio.costo
    }
  }

  def volumenOcupado() = {
    // lo mismo que puse por ahí (ya no me acuerdo en cual de los folds :P), revisen el "sum" con 
    //   la type class Numeric[T] para evitar repertir tanto éste código 
    envios.foldLeft(0.m3) { (volumen, envio) =>
      volumen + envio.volumen
    }
  }

  def duracion: Hora = {
    val distancia = transporte.distanciaEntre(sucursalOrigen, sucursalDestino) * 2 // ida y vuelta
    transporte.velocidad.horasParaRecorrer(distancia)
  }
}