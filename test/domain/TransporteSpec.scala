package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.VolumenM3
import domain.transporte.Camion

class TransporteSpec extends FlatSpec with Matchers {
  "Un transporte" should "poder verificar si puede realizar un envio menor a su capacidad" in {
    var transporte = new Camion

    transporte.puedeTransportarVolumen(new VolumenM3(40)) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad" in {
    var transporte = new Camion

    transporte.puedeTransportarVolumen(new VolumenM3(150)) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 1 carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(40)))

    transporte.puedeTransportarVolumen(new VolumenM3(10)) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 3 carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(10)))
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(10)))
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(10)))

    transporte.puedeTransportarVolumen(new VolumenM3(20)) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(20)))

    transporte.puedeTransportarVolumen(new VolumenM3(25)) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene 2 cargas" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(20)))
    transporte.agregarEnvio(new Envio(null, null, new VolumenM3(20)))

    transporte.puedeTransportarVolumen(new VolumenM3(5)) should be(true)
  }
}