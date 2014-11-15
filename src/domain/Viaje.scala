package domain

import java.util.Date
import unidadmedida.VolumenM3
import exceptions.ValidacionException
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory

class Viaje(var sucursalOrigen: Sucursal, var sucursalDestino: Sucursal, var transporte: Transporte = null, var fechaSalida: Date = new Date) {
  
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
	var envios: Set[Envio] = Set()
	
	def tieneEnvios = !envios.isEmpty

	def agregarEnvio(envio: Envio) = {
	  envios = envios + envio
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
}