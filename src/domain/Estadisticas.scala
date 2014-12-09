package domain

import scala.collection.mutable.HashSet
import unidadmedida.Dinero
import unidadmedida.Hora
import unidadmedida.UnidadesFactory

object Estadisticas {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  // aca murió la solución funcional pura :P
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
    //viajesFiltrados.flatMap(viaje => viaje.envios).size
    viajesFiltrados.foldLeft(0){ (acum,viaje) => acum + viaje.envios.size }

  }
  
  def facturacionTotal(filtro1:Filtro, filtro2: Filtro): Dinero = {
    var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    
    viajesFiltrados.foldLeft(0.pesos){ (acum, viaje) => acum + viaje.precioEnvio}
  }
  
  def tiempoPromedio(filtro1:Filtro, filtro2: Filtro): Hora = {
    var viajesFiltrados = aplicarFiltros(filtro1,filtro2)
    
    var duracion = viajesFiltrados.foldLeft(0.hora){ (acum, viaje) => acum + viaje.duracion}
 
    duracion / viajesFiltrados.size
  }
  
  def aplicarFiltros(filtros: Filtro*): HashSet[Viaje] = {
    
    var viajesFiltrados = filtros.foldLeft(viajesRealizados){ (viajes,filtro) => aplicarFiltro(filtro,viajes) }
    viajesFiltrados
    
  }
    
    def aplicarFiltro(filtro:Filtro,viajes:HashSet[Viaje]):HashSet[Viaje] = {
      // no es necesario definir un var, el "match" ya retorna un valor (como el "if")
      var viajesFiltrados: HashSet[Viaje] = null
      filtro match {
      case FiltroSucursal(sucursal) => viajesFiltrados = viajes.filter(viaje => viaje.sucursalOrigen.equals(sucursal))
      // NO! no filtrar por la clase del objeto transporte!
      case FiltroTransporte(transporte) => viajesFiltrados = viajes.filter(viaje => viaje.transporte.getClass().equals(transporte.getClass()))
      // detalle: pueden buscar con "exists" en vez de mapear y luego buscar por equals
      case FiltroEnvio(tipoEnvio) => viajesFiltrados = viajes.filter(viaje => viaje.envios.map(envio => envio.tipoEnvio).contains(tipoEnvio))
      case FiltroFecha(fechaDesde, fechaHasta) => viajesFiltrados = viajes.filter(viaje => (viaje.fechaSalida.after(fechaDesde)) && viaje.fechaSalida.before(fechaHasta))
      // NO! no matchear por null si quieren un caso genérico usar "_" (igualmente es raro que quieran usar null!)
      case null => viajesFiltrados = viajes
      }
      viajesFiltrados
    }

}