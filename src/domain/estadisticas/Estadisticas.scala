package domain.estadisticas

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.UnidadesFactory
import domain._
import domain.transporte._

object Estadisticas {
  
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
	var viajesRealizados: HashSet[Viaje] = HashSet()
	var sucursales: HashSet[Sucursal] = inicializarSucursales
	
	def inicializarSucursales = {
	  var suc = new HashSet[Sucursal] //RECONTRA hardcodeado papa
	  suc.add(Central)
	  suc.add(Mendoza)
	  suc.add(BahiaBlanca)
	  suc.add(Rio)
	  suc
	}
	
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
	  var costoPaquetes = viajesDelTransporte.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoFacturado }
	  costoPaquetes / viajesDelTransporte.size
	}
	
	def obtenerCostoPromedioViajes(sucursal:Sucursal):Dinero = {
	  var viajesDeSucursal = viajesRealizados.filter(_.sucursalOrigen.equals(sucursal))
	  var costoEnvios = viajesDeSucursal.foldLeft(0.pesos) {(total, viaje) => total + viaje.costoFacturado}
	  if(viajesDeSucursal.size == 0)
	    0.pesos
	    else costoEnvios / viajesDeSucursal.size
	}
}