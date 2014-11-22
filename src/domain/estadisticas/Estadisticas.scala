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

  val costoPromedioViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    var costoPaquetes = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoFacturado }
    if (viajes.isEmpty) 0.pesos
    else costoPaquetes / viajes.size
  }
  
  def costoPromedio(filtros: Filtro*): Dinero = (costoPromedioViajes compose viajesFiltrados)(filtros)

  // ganancia promedio

  val gananciaPromedioViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    var gananciaPromedio = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia }
    if (viajes.isEmpty) 0.pesos
    else gananciaPromedio / viajes.size
  }
  
  def gananciaPromedio(filtros: Filtro*): Dinero = (gananciaPromedioViajes compose viajesFiltrados)(filtros)

  // facturacion total

  def facturacionTotalViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia - viaje.costoFacturado }
  }
  
  def facturacionTotal(filtros: Filtro*): Dinero = (facturacionTotalViajes compose viajesFiltrados)(filtros)
  
  // tiempo promedio
  
  val tiempoPromedioViajes:(HashSet[Viaje] => Hora) = { (viajes) =>
    var tiempoPromedio = viajes.foldLeft(0.horas) { (total, viaje) => total + viaje.duracion }
    if (viajes.isEmpty) 0.horas
    else tiempoPromedio / viajes.size
  }
  
  def tiempoPromedio(filtros: Filtro*): Hora = (tiempoPromedioViajes compose viajesFiltrados)(filtros)

  // cantidad de envios
  
  def enviosRealizados(filtros: Filtro*): Int = viajesFiltrados(filtros:_*).flatMap(_.envios).size

  // cantidad de viajes realizados
  
//  def cantidadViajes(filtros:Filtro*) = (((viaje:HashSet[Viaje]) => viaje.size) compose viajesFiltrados)(filtros)
  def cantidadViajes(filtros:Filtro*) = viajesFiltrados(filtros:_*).size

}