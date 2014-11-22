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
    
  def fixture1 =
    new {
      
    var camion = Camion(List(Normal), false, false)
    var viaje1 = Viaje(camion, Central, Nil, new Date(2014,5,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje1 = viaje1.agregarEnvio(envio1)
    viaje1.costoEnvio should be(10034.pesos)
    
    camion = Camion(List(Normal), false, false)
    var viaje2 = Viaje(camion, Mendoza, Nil, new Date(2014,5,2))
    val envio2 = Envio(Rio, 10.m3, Normal, Otro)
    viaje2 = viaje2.agregarEnvio(envio2)
    viaje2 = viaje2.agregarEnvio(envio2)
    viaje2.costoEnvio should be(10044.pesos)
    
    viaje1.enviar
    viaje2.enviar
    
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje3 = Viaje(avion, Central, Nil, new Date(2014,8,1))
    val envio3 = Envio(Central, 100.m3, Normal, Otro)
    viaje3 = viaje3.agregarEnvio(envio3)
    viaje3.costoEnvio should be(500510.pesos)
    
    viaje3.enviar
     
    avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje4 = Viaje(avion, Central, Nil, new Date(2014,10,1))
    val envio4 = Envio(Central, 100.m3, Normal, Otro)
    viaje4 = viaje4.agregarEnvio(envio4)
    viaje4 = viaje4.agregarEnvio(envio4)
    viaje4.costoEnvio should be(500520.pesos)
    
    viaje4.enviar
    
    camion = Camion(List(Normal), false, false)
    var viaje5 = Viaje(camion, Central, Nil, new Date(2014,11,1))
    val envio5a = Envio(Rio, 10.m3, Normal, Otro)
    val envio5b = Envio(Rio, 10.m3, NecesitaRefrigeracion, Otro)
    viaje5 = viaje5.agregarEnvio(envio5a)
    viaje5 = viaje5.agregarEnvio(envio5b)
    viaje5.costoEnvio should be(10109.pesos)
    
    viaje5.enviar
    
    }
    
  def fixture2 =
    new {
    var camion = Camion(List(Normal,Fragil,Urgente), false, false)
    var viaje6 = Viaje(camion, Mendoza, Nil,null)
    val envio6a = Envio(Rio, 10.m3, Fragil, Otro)
    val envio6b = Envio(Rio, 10.m3, NecesitaRefrigeracion, Otro)
    viaje6 = viaje6.agregarEnvio(envio6a)
    viaje6 = viaje6.agregarEnvio(envio6b)
    viaje6.enviar
    }

  "Costo promedio por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.costoPromedio(FiltroSucursal(Mendoza), null) should be(10044.pesos) 
    Estadisticas.costoPromedio(FiltroSucursal(Central), null) should be(255293.25.pesos) 
  }
  
  "Costo promedio por sucursal y por transporte" should " " in {
    
    val f = fixture1
    val avion = Avion(List(Normal), true, false)
    Estadisticas.costoPromedio(FiltroSucursal(Central),FiltroTransporte(avion)) should be(500515.pesos) 
  }
  
  "Ganancia promedio por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.gananciaPromedio(FiltroSucursal(Mendoza), null) should be((-9884).pesos) 
    Estadisticas.gananciaPromedio(FiltroSucursal(Central), null) should be((-255140.75).pesos) 
  }
  
   "Ganancia promedio por sucursal y por transporte" should " " in {
    
    val f = fixture1
 
    val avion = Avion(List(Normal), true, false)
    Estadisticas.gananciaPromedio(FiltroSucursal(Central),FiltroTransporte(avion)) should be(-500395.pesos) 
  }
   
  "Cantidad de viajes por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.cantidadViajes(FiltroSucursal(Mendoza), null) should be((1)) 
    Estadisticas.cantidadViajes(FiltroSucursal(Central), null) should be((4)) 
  }
  
  "Cantidad de viajes por sucursal y transporte" should " " in {
    
    val f = fixture1
 
    val avion = Avion(List(Normal), true, false)
    Estadisticas.cantidadViajes(FiltroSucursal(Central), FiltroTransporte(avion)) should be((2)) 
  }
   
  "Cantidad de envios por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.cantidadEnvios(FiltroSucursal(Mendoza), null) should be((2)) 
    Estadisticas.cantidadEnvios(FiltroSucursal(Central), null) should be((6)) 
  }
  
  
  "Cantidad de envios por sucursal y transporte" should " " in {
    
    val f = fixture1
 
    val avion = Avion(List(Normal), true, false)
    Estadisticas.cantidadEnvios(FiltroSucursal(Central), FiltroTransporte(avion)) should be((3)) 
  }
  
  "Facturacion total por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.facturacionTotal(FiltroSucursal(Mendoza), null) should be((160.pesos)) 
    Estadisticas.facturacionTotal(FiltroSucursal(Central), null) should be((610.pesos)) 
  }
  
  "Facturacion total por sucursal y por transporte" should " " in {
    
    val f = fixture1
 
    val avion = Avion(List(Normal), true, false)
    Estadisticas.facturacionTotal(FiltroSucursal(Central), FiltroTransporte(avion)) should be((240.pesos)) 
  }
  
  
  "Tiempo promedio por sucursal" should " " in {
    
    val f = fixture1
 
    Estadisticas.tiempoPromedio(FiltroSucursal(Mendoza), null) should be((3.33.hora)) 
    Estadisticas.tiempoPromedio(FiltroSucursal(Central), null) should be((3.665.hora)) 
  }
  
  "Tiempo promedio por sucursal y transporte" should " " in {
    
    val f = fixture1
 
    val avion = Avion(List(Normal), true, false)
    Estadisticas.tiempoPromedio(FiltroSucursal(Central), FiltroTransporte(avion)) should be((4.hora)) 
    
  }
  
  "Cantidad de viajes por transporte" should " " in {
    
    val f1 = fixture1
    val f2 = fixture2
    
    val camion = Camion(List(Normal), true, false)
    val avion = Avion(List(Normal), true, false)
    
    Estadisticas.cantidadViajes(FiltroTransporte(camion), null) should be((4)) 
    Estadisticas.cantidadViajes(FiltroTransporte(avion), null) should be((2)) 
  }
  
  "Cantidad de viajes por transporte y por tipo de envio" should " " in {
    
    val f1 = fixture1
    val f2 = fixture2
    
    val camion = Camion(List(Normal), true, false)
    val avion = Avion(List(Normal), true, false)
    
    Estadisticas.cantidadViajes(FiltroTransporte(camion), FiltroEnvio(Normal)) should be((3))  
  }
  
  
   "Cantidad de viajes por tipo de envio" should " " in {
    
    val f1 = fixture1
    val f2 = fixture2
    
    Estadisticas.cantidadViajes(FiltroEnvio(Normal), null) should be((5)) 
    Estadisticas.cantidadViajes(FiltroEnvio(NecesitaRefrigeracion), null) should be((2)) 
  }
  
  "Cantidad de viajes por tipo de envio y rango de fechas" should " " in {
    
    val f1 = fixture1
    val f2 = fixture2
    
    Estadisticas.cantidadViajes(FiltroEnvio(Normal),FiltroFecha(new Date(2014,4,1),new Date(2014,8,20))) should be((3))  
  }
  
  
}