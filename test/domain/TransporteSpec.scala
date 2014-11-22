package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import domain.Transporte
import domain.Sucursal
import domain.Viaje
import unidadmedida.UnidadesFactory
import exceptions.ValidacionException
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import unidadmedida.VolumenM3
import exceptions.TransporteNoPoseeInfraestructura

class TransporteSpec2 extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)

  //def fixture =
  //  new {
  //    val sucursal1 = Sucursal("sucursal1",100.m3,"Argentina)
  //}

  "Un Camion" should "tiene una capacidad de 45 m3" in {

    val camion = Camion(List(Urgente, Normal, Fragil), false, false)

    camion.capacidad should be(45.m3)
  }

  "Un camion" should "aguanta dos envios de 10 m3" in {

    var camion = Camion(List(Urgente, Normal, Fragil), false, false)

    var viaje = Viaje(camion, Central, Nil, null)

    val envio = Envio(Rio, 10.m3, Urgente, null)

    viaje = viaje.agregarEnvio(envio)

    viaje.envios should be(List(envio))

    viaje = viaje.agregarEnvio(envio)

    viaje.envios should be(List(envio, envio))

  }

  "Un camion" should "no aguanta 2 envios de 30 m3" in {

    var camion = Camion(List(Urgente, Normal, Fragil), false, false)

    var viaje = Viaje(camion, Central, Nil, null)

    val envio = Envio(null, 30.m3, Urgente, null)

    viaje = viaje.agregarEnvio(envio)
    //camion = camion.agregarEnvio(envio)  

    intercept[TransporteTieneVolumenInsuficienteParaRealizarElEnvio] {
      viaje = viaje.agregarEnvio(envio)
    }

  }

  "Un Avion" should "no puede transportar un envio que necesita refrigeracion" in {

    var avion = Avion(List(Urgente, Normal, Fragil), false, false)

    var viaje = Viaje(avion, Central, Nil, null)

    val envio = Envio(null, 30.m3, NecesitaRefrigeracion, null)

    intercept[TransporteNoSoportaElTipoEnvioEspecificado] {
      viaje = viaje.agregarEnvio(envio)
    }

  }

  "Un transporte" should "no puede llevar envios de dos sucursales distintas" in {

    var avion = Avion(List(Urgente, Normal, Fragil), false, false)

    var viaje = Viaje(avion, Central, Nil, null)

    val envio1 = Envio(Mendoza, 30.m3, Normal, null)
    val envio2 = Envio(Rio, 30.m3, Normal, null)

    viaje = viaje.agregarEnvio(envio1)

    intercept[TransporteNoSeDirigeALaSucursalDeDestinoEspecificada] {
      viaje = viaje.agregarEnvio(envio2)
    }

  }

}