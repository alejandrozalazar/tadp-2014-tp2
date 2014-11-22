package domain.estadisticas

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.UnidadesFactory
import domain._
import domain.transporte._
import unidadmedida.Hora

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
  
  def vaciar = {
    viajesRealizados = new HashSet()
    sucursales = new HashSet()
  }

  def agregarViajeRealizado(viaje: Viaje) = {
    viajesRealizados.add(viaje)
  }

  def costoEnviosViajes(transporte: Transporte): Dinero = {
    var viajesDelTransporte = viajesRealizados.filter(_.transporte.equals(transporte))
    var costoPaquetes = viajesDelTransporte.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoPaquetes }
    costoPaquetes / viajesDelTransporte.size
  }
  
  def viajesFiltrados(filtros: Filtro*) = {
    filtros.foldLeft(viajesRealizados){ (viajes, filtro) => filtro.filtrar(viajes) }
  }

  // costo promedio

  def costoPromedioViajes(viajes: HashSet[Viaje]): Dinero = {
    var costoPaquetes = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoFacturado }
    if (viajes.isEmpty) 0.pesos
    else costoPaquetes / viajes.size
  }
  
  def costoPromedioViajes(filtros: Filtro*): Dinero = {
    costoPromedioViajes(viajesFiltrados(filtros:_*))
  }

  // ganancia promedio

  def gananciaPromedio(viajes: HashSet[Viaje]): Dinero = {
    var gananciaPromedio = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia }
    if (viajes.isEmpty) 0.pesos
    else gananciaPromedio / viajes.size
  }
  
  def gananciaPromedio(filtros: Filtro*): Dinero = {
    gananciaPromedio(viajesFiltrados(filtros:_*))
  }

  // facturacion total

  def facturacionTotal(viajes: HashSet[Viaje]): Dinero = {
    viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia - viaje.costoFacturado }
  }
  
  def facturacionTotal(filtros: Filtro*): Dinero = {
    facturacionTotal(viajesFiltrados(filtros:_*))
  }
  
  // tiempo promedio
  
  def tiempoPromedio(viajes: HashSet[Viaje]): Hora = {
    var tiempoPromedio = viajes.foldLeft(0.horas) { (total, viaje) => total + viaje.duracion }
    if (viajes.isEmpty) 0.horas
    else tiempoPromedio / viajes.size
  }
  
  def tiempoPromedio(filtros: Filtro*): Hora = {
    tiempoPromedio(viajesFiltrados(filtros:_*))
  }

  // cantidad de envios
  
  def enviosRealizados(viajes: HashSet[Viaje]): Int = {
    viajes.flatMap(_.envios).size
  }
  
  def enviosRealizados(filtros: Filtro*): Int = {
    enviosRealizados(viajesFiltrados(filtros:_*))
  }

  // cantidad de viajes realizados
  
  def viajesRealizados(viajes: HashSet[Viaje]): Int = {
    viajes.size
  }
  
  def viajesRealizados(filtros: Filtro*): Int = {
    viajesRealizados(viajesFiltrados(filtros:_*))
  }
}