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

case class Viaje(val sucursalOrigen: Sucursal, val sucursalDestino: Sucursal, val transporte: Transporte = null, val fechaSalida: GregorianCalendar = new GregorianCalendar(2014, 1, 1), val envios: Set[Envio] = Set(), val costoFacturado:Dinero = Dinero(0), val ganancia:Dinero = Dinero(0)) {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  def tieneEnvios = !envios.isEmpty

  def agregarEnvio(envio: Envio):Viaje = {
    this.copy(envios = this.envios + envio)
  }

  def costoPaquetes(): Dinero = {
    envios.foldLeft(0.pesos) { (costoTotal, envio) =>
      costoTotal + envio.costo
    }
  }

  def volumenOcupado() = {
    envios.foldLeft(0.m3) { (volumen, envio) =>
      volumen + envio.volumen
    }
  }

  def duracion: Hora = {
    val distancia = transporte.distanciaEntre(sucursalOrigen, sucursalDestino) * 2 // ida y vuelta
    transporte.velocidad.horasParaRecorrer(distancia)
  }
}