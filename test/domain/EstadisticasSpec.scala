package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import unidadmedida.Dinero
import domain.transporte.Camion
import domain.transporte.Furgoneta
import domain.transporte.Avion
import java.util.Date
import domain.estadisticas._
import org.scalatest.BeforeAndAfter

class EstadisticasSpec extends FlatSpec with Matchers with BeforeAndAfter {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  before {
    Estadisticas.vaciar
  }

  "El generador de estadisticas" should "mostrarme el costo de los envios" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    Estadisticas.costoEnviosViajes(camion) should be(10.pesos)
  }

  //	6. Estadísticas
  //
  //El sistema deberá proveer información estadística con base en la totalidad de los envíos realizados históricamente.
  //
  //Se desea poder combinar diferentes tipos de estadísticas, para comparar diferentes tipos de información:
  //
  //* Comparar las diferentes sucursales entre sí.
  //* Comparar los tipos de transporte entre sí.
  //* Comparar los tipos de envíos entre sí.
  //
  //Luego se podrán establecer filtros sobre esa información. Los filtros pueden ser por fecha en que salío el transporte
  //de la sucursal y/o cualquier valor por el que se puede agrupar. Por ejemplo:
  //
  //* Una comparación entre sucursales se puede restringir un sólo tipo de transporte.
  //* Una comparación entre tipos de transporte se puede restringir por tipo de envío.
  //* Una comparación entre tipos de envío se puede restringir por un rango de fechas.
  //
  //Las comparaciones que se desean poder obtener son:
  //
  //* Costo promedio de los viajes.

  "El generador de estadisticas" should "comparar sucursales por costo promedio de viajes" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtroMendoza= FiltroSucursal(Mendoza)
    val filtroCentral= FiltroSucursal(Central)
    Estadisticas.costoPromedioViajes(filtroMendoza) should be(10034.pesos)
    Estadisticas.costoPromedioViajes(filtroCentral) should be(0.pesos)
  }

  "El generador de estadisticas" should "mostrarme el costo promedio de los envios por transporte" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtroTransporte = FiltroTransporte(camion)
    Estadisticas.costoPromedioViajes(filtroTransporte) should be(10039.pesos)
  }

  "El generador de estadisticas" should "mostrarme el costo promedio de los envios por tipo de envio" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtroTipoEnvio = FiltroTipoEnvio(Normal)
    Estadisticas.costoPromedioViajes(filtroTipoEnvio) should be(10034.pesos)
  }

  //* Ganancia promedio de los viajes.

  "El generador de estadisticas" should "mostrarme la ganancia promedio de los envios por tipo de envio" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtroTipoEnvio = FiltroTipoEnvio(Normal)
    Estadisticas.gananciaPromedio(filtroTipoEnvio).round should be(0.pesos)
  }

  "El generador de estadisticas" should "mostrarme la ganancia promedio de los envios por transporte" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtroTransporte = FiltroTransporte(camion)
    Estadisticas.gananciaPromedio(filtroTransporte) should be(-5012.pesos)
  }

  "El generador de estadisticas" should "comparar sucursales por ganancia promedio de viajes" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtroMendoza= FiltroSucursal(Mendoza)
    val filtroCentral= FiltroSucursal(Central)
    Estadisticas.gananciaPromedio(filtroMendoza) should be(-5012.pesos)
    Estadisticas.gananciaPromedio(filtroCentral) should be(0.pesos)
  }

  //* Tiempo promedio de los viajes.
  "El generador de estadisticas" should "proveer el tiempo promedio de viajes" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtro = FiltroOrigenDestino(Mendoza, Central)
    val extractedLocalValue = Estadisticas.tiempoPromedio(filtro) 
    extractedLocalValue.round should be(3.horas)
  }
  
  "El generador de estadisticas" should "proveer el tiempo promedio de viajes incluyendo aereos" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Central, Rio, 1.m3, Normal))
    camion.realizarViaje
    val avion = new Avion
    avion.agregarEnvio(new Envio(Central, Rio, 1.m3, Normal))
    avion.realizarViaje
    val filtro = FiltroOrigenDestino(Central, Rio)
    val extractedLocalValue = Estadisticas.tiempoPromedio(filtro) 
    extractedLocalValue should be(3.6686666666666667.horas)
  }

  //* Cantidad de envíos

  "El generador de estadisticas" should "mostrarme la cantidad de envios realizados por tipo de envio" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Urgente))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTipoEnvio(Normal)
    Estadisticas.enviosRealizados(filtro) should be(2)
  }

  "El generador de estadisticas" should "mostrarme la cantidad de envios realizados por transporte" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTransporte(camion)
    Estadisticas.enviosRealizados(filtro) should be(2)
  }

  "El generador de estadisticas" should "mostrarme la cantidad de envios realizados por sucursal" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtroMendoza= FiltroSucursal(Mendoza)
    val filtroCentral= FiltroSucursal(Central)
    Estadisticas.enviosRealizados(filtroMendoza) should be(2)
    Estadisticas.enviosRealizados(filtroCentral) should be(0)
  }

  //* Cantidad de viajes

  "El generador de estadisticas" should "mostrarme la cantidad de viajes realizados por tipo de envio" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTipoEnvio(Normal)
    Estadisticas.viajesRealizados(filtro) should be(1)
  }

  "El generador de estadisticas" should "mostrarme la cantidad de viajes realizados por transporte" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTransporte(camion)
    Estadisticas.viajesRealizados(filtro) should be(2)
  }

  "El generador de estadisticas" should "mostrarme la cantidad de viajes realizados por sucursal" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtroMendoza= FiltroSucursal(Mendoza)
    val filtroCentral= FiltroSucursal(Central)
    Estadisticas.viajesRealizados(filtroMendoza) should be(2)
    Estadisticas.viajesRealizados(filtroCentral) should be(0)
  }

  //* Facturacion total

  "El generador de estadisticas" should "mostrarme la facturacion total por tipo de envio" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTipoEnvio(Normal)
    Estadisticas.facturacionTotal(filtro).round should be(-10034.pesos)
  }

  "El generador de estadisticas" should "mostrarme la facturacion total por transporte" in {
    var camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    camion.realizarViaje
    val filtro = FiltroTransporte(camion)
    Estadisticas.facturacionTotal(filtro) should be(-30102.pesos)
  }

  "El generador de estadisticas" should "mostrarme la facturacion total por sucrusal" in {
    val camion = new Camion
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion.realizarViaje
    val filtroMendoza= FiltroSucursal(Mendoza)
    val filtroCentral= FiltroSucursal(Central)
    Estadisticas.facturacionTotal(filtroMendoza) should be(-30092.pesos)
    Estadisticas.facturacionTotal(filtroCentral) should be(0.pesos)
  }

  //Algunos ejemplos de estadísticas que se podrían obtener con estas combinaciones:
  //
  //* Dada una sucursal la cantidad de viajes según cada tipos de transportes
  //* La facturación total (en un rango de fechas) para cada tipo de transporte para todo el sistema
  //* El tiempo (o costo) promedio de cada tipo de transporte.
  //* La facturacíon total de cada compañía por cada sucursal.

}