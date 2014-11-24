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

  val costoPromedioViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    var costoPaquetes = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoFacturado }
    if (viajes.isEmpty) 0.pesos
    else costoPaquetes / viajes.size
  }

  def costoPromedio(f: filtro): Dinero = (costoPromedioViajes compose f)(viajesRealizados)

  // ganancia promedio

  val gananciaPromedioViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    var gananciaPromedio = viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia }
    if (viajes.isEmpty) 0.pesos
    else gananciaPromedio / viajes.size
  }
  
  def gananciaPromedio(f: filtro): Dinero = (gananciaPromedioViajes compose f)(viajesRealizados)

  // facturacion total

  def facturacionTotalViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    viajes.foldLeft(0.pesos) { (total, viaje) => total + viaje.ganancia - viaje.costoFacturado }
  }

  def facturacionTotal(f: filtro): Dinero = (facturacionTotalViajes compose f)(viajesRealizados)
  
  // tiempo promedio
  
  val tiempoPromedioViajes:(HashSet[Viaje] => Hora) = { (viajes) =>
    var tiempoPromedio = viajes.foldLeft(0.horas) { (total, viaje) => total + viaje.duracion }
    if (viajes.isEmpty) 0.horas
    else tiempoPromedio / viajes.size
  }

  def tiempoPromedio(f: filtro): Hora = (tiempoPromedioViajes compose f)(viajesRealizados)

  // cantidad de envios

  def enviosRealizados(f: filtro): Int = f(viajesRealizados).flatMap(_.envios).size

  // cantidad de viajes realizados

  def cantidadViajes(f: filtro): Int = f(viajesRealizados).size

}