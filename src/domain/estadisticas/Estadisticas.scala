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

  // costo promedio

  def costoPromedio(viajes: HashSet[Viaje]): Dinero = {
    var costoPaquetes = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoFacturado }
    if (viajes.isEmpty) 0.pesos
    else costoPaquetes / viajes.size
  }

  def costoPromedioViajes(transporte: Transporte): Dinero = {
    costoPromedio(viajesRealizados.filter(_.transporte.equals(transporte)))
  }

  def costoPromedioViajes(sucursal: Sucursal): Dinero = {
    costoPromedio(viajesRealizados.filter(_.sucursalOrigen.equals(sucursal)))
  }

  def costoPromedioViajes(tipo: TipoEnvio): Dinero = {
    costoPromedio(viajesRealizados.filter(_.envios.exists(_.tipoEnvio.equals(tipo))))
  }

  // ganancia promedio

  def gananciaPromedio(viajes: HashSet[Viaje]): Dinero = {
    var gananciaPromedio = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia }
    if (viajes.isEmpty) 0.pesos
    else gananciaPromedio / viajes.size
  }

  def gananciaPromedioViajes(transporte: Transporte): Dinero = {
    gananciaPromedio(viajesRealizados.filter(_.transporte.equals(transporte)))
  }

  def gananciaPromedioViajes(sucursal: Sucursal): Dinero = {
    gananciaPromedio(viajesRealizados.filter(_.sucursalOrigen.equals(sucursal)))
  }

  def gananciaPromedioViajes(tipo: TipoEnvio): Dinero = {
    gananciaPromedio(viajesRealizados.filter(_.envios.exists(_.tipoEnvio.equals(tipo))))
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
}