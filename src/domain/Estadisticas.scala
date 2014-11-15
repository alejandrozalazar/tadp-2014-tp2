package domain

import scala.collection.mutable.HashSet

object Estadisticas {
	var viajesRealizados: HashSet[Viaje] = HashSet()
	
	def agregarViajeRealizado(viaje: Viaje) = {
	  viajesRealizados.add(viaje)
	}
}