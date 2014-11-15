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

class EstadisticasSpec extends FlatSpec with Matchers {
  
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
	"El generador de estadisticas" should "mostrarme el costo promedio de los viajes" in {
	  var camion = new Camion
	  camion.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
	  camion.realizarViaje
	  Estadisticas.costoPromedioViajes(camion) should be(10.pesos)
	}
}