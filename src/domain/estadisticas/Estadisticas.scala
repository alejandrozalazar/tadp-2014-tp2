package domain.estadisticas

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.UnidadesFactory
import domain._
import domain.transporte._
import unidadmedida.Hora

// "object" lo hace intesteable, como voy a hacer en mi código para testear los métodos que usen
// a las estadísticas sin tener que testear todas las combinaciones de estadísticas a la vez?!
object Estadisticas {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  var viajesRealizados: HashSet[Viaje] = HashSet()
  var sucursales: HashSet[Sucursal] = inicializarSucursales

  def inicializarSucursales = {
    var suc = new HashSet[Sucursal] //RECONTRA hardcodeado papa
    // ojo, estan definiendo valores de testing en codigo productivo!
    suc.add(Central)
    suc.add(Mendoza)
    suc.add(BahiaBlanca)
    suc.add(Rio)
    suc
    // detalle de scala, pueden usar Set(1,2,3) sin necesidad de usar "add"
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
  
  // casualmente todos sus "filtros" hacen x.filter => filter debería estar acá y el parámetro es el predicate
  def gananciaPromedio(f: filtro): Dinero = (gananciaPromedioViajes compose f)(viajesRealizados)

  // facturacion total

  def facturacionTotalViajes:(HashSet[Viaje] => Dinero) = { (viajes) =>
    // hay muchos folds con 0, pueden usar el "sum" de las colecciones, si usan un tipo Numeric;
    //  o pueden implementar su propio implicit Numeric[T] para sus tipos (type class)
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