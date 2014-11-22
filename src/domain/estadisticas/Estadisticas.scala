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

  def facturacionTotal(transporte: Transporte): Dinero = {
    facturacionTotal(viajesRealizados.filter(_.transporte.equals(transporte)))
  }

  def facturacionTotal(sucursal: Sucursal): Dinero = {
    facturacionTotal(viajesRealizados.filter(_.sucursalOrigen.equals(sucursal)))
  }

  def facturacionTotal(tipo: TipoEnvio): Dinero = {
    facturacionTotal(viajesRealizados.filter(_.envios.exists(_.tipoEnvio.equals(tipo))))
  }
  
  // tiempo promedio

  def tiempoPromedioViajesEntre(sucursalOrigen: Sucursal, sucursalDestino: Sucursal): Hora = {
    val viajesRealizadosEntreSucursales = viajesRealizados.filter(_.sucursalOrigen.equals(sucursalOrigen)).filter(_.sucursalDestino.equals(sucursalDestino))

    var tiempoPromedio = viajesRealizadosEntreSucursales.foldLeft(0.horas) { (total, viaje) => total + viaje.duracion }
    if (viajesRealizadosEntreSucursales.isEmpty) 0.horas
    else tiempoPromedio / viajesRealizadosEntreSucursales.size
  }

  // cantidad de envios

  def enviosRealizados(transporte: Transporte): Int = {
    viajesRealizados.filter(_.transporte.equals(transporte)).flatMap(_.envios).size
  }

  def enviosRealizados(sucursal: Sucursal): Int = {
    viajesRealizados.filter(_.sucursalOrigen.equals(sucursal)).flatMap(_.envios).size
  }

  def enviosRealizados(tipo: TipoEnvio): Int = {
    viajesRealizados.flatMap(_.envios).filter(_.tipoEnvio.equals(tipo)).size
  }

  // cantidad de viajes realizados

  def viajesRealizados(transporte: Transporte): Int = {
    viajesRealizados.filter(_.transporte.equals(transporte)).size
  }

  def viajesRealizados(sucursal: Sucursal): Int = {
    viajesRealizados.filter(_.sucursalOrigen.equals(sucursal)).size
  }

  def viajesRealizados(tipo: TipoEnvio): Int = {
    viajesRealizados.filter(_.envios.exists(_.tipoEnvio.equals(tipo))).size
  }
}