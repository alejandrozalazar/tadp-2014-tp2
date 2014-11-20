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
import domain.estadisticas.Estadisticas

class EstadisticasSpec extends FlatSpec with Matchers {
  
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  	"El generador de estadisticas" should "comparar sucursales por costo promedio de viajes" in {
	  val camion = new Camion
	  camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
	  camion.realizarViaje
	  camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
	  camion.realizarViaje
	  Estadisticas.costoPromedioViajes(Mendoza) should be(10034.pesos)
	  Estadisticas.costoPromedioViajes(Central) should be (0.pesos)
    }
  
  
	"El generador de estadisticas" should "mostrarme el costo de los envios" in {
	  var camion = new Camion
	  camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
	  camion.realizarViaje
	  Estadisticas.costoEnviosViajes(camion) should be(10.pesos)
	}
  
	"El generador de estadisticas" should "mostrarme el costo promedio de los envios" in {
	  var camion = new Camion
	  camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
	  camion.realizarViaje
	  camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
	  camion.realizarViaje
	  Estadisticas.costoPromedioViajes(camion) should be(10039.pesos)
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
	//
	//de la sucursal y/o cualquier valor por el que se puede agrupar. Por ejemplo:
	//
	//* Una comparación entre sucursales se puede restringir un sólo tipo de transporte.
	//* Una comparación entre tipos de transporte se puede restringir por tipo de envío.
	//* Una comparación entre tipos de envío se puede restringir por un rango de fechas.
	//
	//Las comparaciones que se desean poder obtener son:
	//
	//* Costo promedio de los viajes.
	//* Ganancia promedio de los viajes.
	//* Tiempo promedio de los viajes.
	//* Cantidad de envíos
	//* Cantidad de viajes
	//* Facturación total.
	//
	//Algunos ejemplos de estadísticas que se podrían obtener con estas combinaciones:
	//
	//* Dada una sucursal la cantidad de viajes según cada tipos de
	//* La facturación total (en un rango de fechas) para cada tipo de transporte para todo el sistema
	//* El tiempo (o costo) promedio de cada tipo de transporte.
	//* La facturacíon total de cada compañía por cada sucursal.

}