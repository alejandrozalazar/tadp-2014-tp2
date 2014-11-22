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

  "Un paquete normal por camion" should "tiene un costo de 10034 " in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, null)
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10034.pesos)
  }

  "Dos paquetes normales por camion" should "tienen un costo de 10044 " in {

    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, null)
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(10044.pesos)
  }

  "Paquetes con refrigeracion" should "agregan 5 pesos de costo " in {
    var camion = Camion(List(Normal), false, false)
    var viaje = Viaje(camion, Central, Nil, null)
    val envio1 = Envio(Rio, 10.m3, Normal, Otro)
    val envio2 = Envio(Rio, 10.m3, NecesitaRefrigeracion, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio2)
    viaje.costoEnvio should be(10109.pesos)
  }

  "Un paquete normal por avion" should "tienen un costo de 500.510 " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, null)
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(500510.pesos)
  }

  "Dos paquetes normales por avion" should "tienen un costo de 500.520 " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, null)
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(500520.pesos)
  }
  "Un avion que viaja a otro pais" should "recargar un 10% " in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Rio, Nil, null)
    val envio1 = Envio(Central, 100.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(550572.pesos)
  }

  "Un avion cargado menos del 20%" should "multiplicar por 3 el costo" in {
    var avion = Avion(List(Urgente, Normal, Fragil), false, false)
    var viaje = Viaje(avion, Central, Nil, null)
    val envio1 = Envio(Rio, 5.m3, Normal, Otro)
    viaje = viaje.agregarEnvio(envio1)
    viaje = viaje.agregarEnvio(envio1)
    viaje.costoEnvio should be(1651716.pesos)
  }
}