package domain

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.UnidadesFactory

object Estadisticas {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  var viajesRealizados: HashSet[Viaje] = HashSet()

  def agregarViajeRealizado(viaje: Viaje) = {
    viajesRealizados.add(viaje)
  }

  def costoPromedio(filtro1: Filtro, filtro2: Filtro): Dinero = {

    var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    var costoPaquetes = viajesFiltrados.foldLeft(0.pesos) { (total, viaje) => total + viaje.costoEnvio }
    costoPaquetes / viajesFiltrados.size

  }

  def gananciaPromedio(filtro1: Filtro, filtro2: Filtro): Dinero = {

    var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    var gananciaPaquetes = viajesFiltrados.foldLeft(0.pesos) { (total, viaje) => total + viaje.gananciaEnvio }
    gananciaPaquetes / viajesFiltrados.size

  }

  def cantidadViajes(filtro1: Filtro, filtro2: Filtro): Int = {
   var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    viajesFiltrados.size

  }

  def cantidadEnvios(filtro1: Filtro, filtro2: Filtro): Int = {
    var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    viajesFiltrados.flatMap(viaje => viaje.envios).size

  }
  
  def aplicarFiltros(filtro1: Filtro, filtro2: Filtro): HashSet[Viaje] = {
    
    var viajesFiltrados = viajesRealizados
    viajesFiltrados = aplicarFiltro(filtro1,viajesFiltrados)
    viajesFiltrados = aplicarFiltro(filtro2,viajesFiltrados)
    viajesFiltrados
    
  }
    
    def aplicarFiltro(filtro:Filtro,viajes:HashSet[Viaje]):HashSet[Viaje] = {
      var viajesFiltrados: HashSet[Viaje] = null
      filtro match {
      case FiltroSucursal(sucursal) => viajesFiltrados = viajes.filter(viaje => viaje.sucursalOrigen.equals(sucursal))
      case FiltroTransporte(transporte) => viajesFiltrados = viajes.filter(viaje => viaje.transporte.getClass().equals(transporte.getClass()))
      case FiltroEnvio(tipoEnvio) => viajesFiltrados = viajes.filter(viaje => viaje.envios.map(envio => envio.tipoEnvio).contains(tipoEnvio))
      case FiltroFecha(fechaDesde, fechaHasta) => viajesFiltrados = viajes.filter(viaje => (viaje.fechaSalida.after(fechaDesde)) && viaje.fechaSalida.before(fechaHasta))
      case null => viajesFiltrados = viajes
      }
      viajesFiltrados
    }

}