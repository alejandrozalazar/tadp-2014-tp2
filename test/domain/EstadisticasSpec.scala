package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.Transporte
import domain.Viaje
import java.util.Date

class EstadisticasSpec extends FlatSpec with Matchers {

    implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
     implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
    
  def fixture =
    new {
      
    var camion = Camion(List(Normal), false, false)
    var viaje1 = Viaje(camion, Central, Nil, null)
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje1 = viaje1.agregarEnvio(envio1)
    viaje1.costoEnvio should be(10034.pesos)
    
    camion = Camion(List(Normal), false, false)
    var viaje2 = Viaje(camion, Mendoza, Nil, null)
    val envio2 = Envio(Rio, 10.m3, Normal, Otro)
    viaje2 = viaje2.agregarEnvio(envio2)
    viaje2 = viaje2.agregarEnvio(envio2)
    viaje2.costoEnvio should be(10044.pesos)
    
    viaje1.enviar
    viaje2.enviar
    
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje3 = Viaje(avion, Central, Nil, null)
    val envio3 = Envio(Central, 100.m3, Normal, Otro)
    viaje3 = viaje3.agregarEnvio(envio3)
    viaje3.costoEnvio should be(500510.pesos)
    
    viaje3.enviar
     
    avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje4 = Viaje(avion, Central, Nil, null)
    val envio4 = Envio(Central, 100.m3, Normal, Otro)
    viaje4 = viaje4.agregarEnvio(envio4)
    viaje4 = viaje4.agregarEnvio(envio4)
    viaje4.costoEnvio should be(500520.pesos)
    
    viaje4.enviar
    
    camion = Camion(List(Normal), false, false)
    var viaje5 = Viaje(camion, Central, Nil, null)
    val envio5a = Envio(Rio, 10.m3, Normal, Otro)
    val envio5b = Envio(Rio, 10.m3, NecesitaRefrigeracion, Otro)
    viaje5 = viaje5.agregarEnvio(envio5a)
    viaje5 = viaje5.agregarEnvio(envio5b)
    viaje5.costoEnvio should be(10109.pesos)
    
    viaje5.enviar
    
    }

  "Costo promedio por sucursal" should " " in {
    
    val f = fixture
 
    Estadisticas.costoPromedio(FiltroSucursal(Mendoza), null) should be(10044.pesos) 
    Estadisticas.costoPromedio(FiltroSucursal(Central), null) should be(255293.25.pesos) 
  }
  
  "Costo promedio por sucursal y por transporte" should " " in {
    
    val f = fixture
    val avion = Avion(List(Normal), true, false)
    println("Filtro doble")
    Estadisticas.costoPromedio(FiltroSucursal(Central),FiltroTransporte(avion)) should be(500515.pesos) 
  }
   
}