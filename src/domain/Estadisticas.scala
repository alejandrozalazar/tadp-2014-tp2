package domain

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.UnidadesFactory

object Estadisticas {
  /*
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
	var viajesRealizados: HashSet[Viaje] = HashSet()
	
	def agregarViajeRealizado(viaje: Viaje) = {
	  viajesRealizados.add(viaje)
	}
	
	def costoEnviosViajes(transporte: Transporte):Dinero = {
	  var viajesDelTransporte = viajesRealizados.filter(_.transporte.equals(transporte))
	  var costoPaquetes = viajesDelTransporte.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoPaquetes }
	  costoPaquetes / viajesDelTransporte.size
	}
	
	def costoPromedioViajes(transporte: Transporte):Dinero = {
	  var viajesDelTransporte = viajesRealizados.filter(_.transporte.equals(transporte))
	  var costoPaquetes = viajesDelTransporte.foldLeft(0.pesos) { (total, viaje) => total + viaje.costo(transporte) }
	  costoPaquetes / viajesDelTransporte.size
	}
	*/
}