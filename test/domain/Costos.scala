package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.transporte.Camion
import domain.transporte.Furgoneta
import domain.transporte.Avion
import java.util.Date

class CostosSpec extends FlatSpec with Matchers {
  
  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
  
 "Un paquete normal por camion" should "tiene un costo de 10034 " in {
   var camion = Camion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Rio,10.m3,Normal,Otro) 
   camion = camion.agregarEnvio(envio1)
   camion.costoEnvio should be(10034.pesos)     
  }
  
  "Dos paquetes normales por camion" should "tienen un costo de 10044 " in {
   var camion = Camion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Rio,10.m3,Normal,Otro) 
   camion = camion.agregarEnvio(envio1)
   camion = camion.agregarEnvio(envio1)
   camion.costoEnvio should be(10044.pesos)   
  }
  
 "Paquetes con refrigeracion" should "agregan 5 pesos de costo " in {
   var camion = Camion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Rio,10.m3,Normal,Otro) 
   val envio2 = Envio(Rio,10.m3,NecesitaRefrigeracion,Otro)
   camion = camion.agregarEnvio(envio1)
   camion = camion.agregarEnvio(envio2)
   camion.costoEnvio should be(10109.pesos)   
  }
 
  "Un paquete normal por avion" should "tienen un costo de 500.510 " in {
   var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Central,100.m3,Normal,Otro) 
   avion = avion.agregarEnvio(envio1)
   avion.costoEnvio should be(500510.pesos)   
  }
  
  "Dos paquetes normales por avion" should "tienen un costo de 500.520 " in {
   var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Central,100.m3,Normal,Otro) 
   avion = avion.agregarEnvio(envio1)
   avion = avion.agregarEnvio(envio1)
   avion.costoEnvio should be(500520.pesos)   
  }
  "Un avion que viaja a otro pais" should "recargar un 10% " in {
   var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Rio,100.m3,Normal,Otro) 
   avion = avion.agregarEnvio(envio1)
   avion = avion.agregarEnvio(envio1)
   avion.costoEnvio should be(550572.pesos)   
  }
  
  "Un avion cargado menos del 20%" should "multiplicar por 3 el costo" in {
    var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
   val envio1 = Envio(Rio,5.m3,Normal,Otro) 
   avion = avion.agregarEnvio(envio1)
   avion = avion.agregarEnvio(envio1)
   avion.costoEnvio should be(1651716.pesos)   
  }
}