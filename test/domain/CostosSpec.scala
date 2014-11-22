package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.Transporte
import domain.Viaje
import java.util.Date

class CostosSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  "Un paquete normal por camion" should "tiene un costo de 10034 " in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10034.pesos)
  }

  "Dos paquetes normales por camion" should "tienen un costo de 10044 " in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10044.pesos)
  }

  "Paquetes con refrigeracion" should "agregan 5 pesos de costo " in {
    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    val envio2 = Envio(Rio, 10.m3, NecesitaRefrigeracion, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio2)
    viaje.costoEnvio should be(10109.pesos)
  }
  
  "Dos paquetes normales por furgoneta" should "tiene un costo de " in {
    var furgoneta = Furgoneta(List(Normal),false,false)
    var viaje = Viaje(furgoneta, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 2.m3, Normal, Otro)
    val envio2 = Envio(Rio, 2.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio2)
    viaje.costoEnvio should be(4032.pesos)
  }

  "Un paquete normal por avion" should "tienen un costo de 500.510 " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(500510.pesos)
  }

  "Dos paquetes normales por avion" should "tienen un costo de 500.520 " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(500520.pesos)
  }
  
  "Un camion que viaja a Central en la ultima semana del mes" should "reducir el costo un 20%" in {
    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Rio, Nil, new Date(2014,1,26))
    val envio1 = Envio(Central, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10244.88.pesos)
    
  }
  
  "Un avion que viaja a otro pais" should "recargar un 10% " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Rio, Nil, new Date(2014,1,1))
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(550572.pesos)
  }

  "Un avion que viaja a Central luego del día 20" should "aumentar su costo un 20%" in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, new Date(2014,1,25))
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(600624.pesos)
  }
  
  "Un avion cargado menos del 20%" should "multiplicar por 3 el costo" in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 5.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(1651716.pesos)
  }
  
  "Una furgoneta cargada menos del 20% y no lleva 3 o más paquetes urgentes" should "multiplican su costo por 2" in {
    var furgoneta = Furgoneta(List(Normal),false,false)
    var viaje = Viaje(furgoneta, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 0.5.m3, Normal, Otro)
    val envio2 = Envio(Rio, 0.5.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio2)
    viaje.costoEnvio should be(8064.pesos)
  }
  
  "Un camion cargado menos del  20% y va ni viene de centra" should "aumeta su costo proporcional al volumen libre " in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Mendoza, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 1.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10490.4.pesos)
  }
  
  "Un transporte con GPS" should "suman 5$ por km recorrido" in {

    var camion = Camion(List(Normal), true, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10144.pesos)
  }
  
  "Un transporte con seguimiento satelital" should "suma 3.74$ por km recorrido" in {

    var camion = Camion(List(Normal), false, true)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10792.pesos)
  }
  
  "Los viajes con sustancias peligrosas" should "suman 600$ al costo" in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, SustanciaPeligrosa)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10644.pesos)
  }
  
  "Los viajes con de menos de 200 km con animales" should "suman 86$" in {
    
    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Normal, Animal)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10130.pesos)
  }
  
  "Un camion con envios urgentes y animales" should "aumentan su costo en proporcion al volumen que llevan" in {

    var camion = Camion(List(Normal,Urgente), false, false)
    var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
    val envio1 = Envio(Rio, 10.m3, Urgente, SustanciaPeligrosa)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10665.33.pesos)
  }

}