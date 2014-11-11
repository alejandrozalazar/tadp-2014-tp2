package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import domain.transporte.Avion
import domain.transporte.Camion
import domain.transporte.Furgoneta
import unidadmedida.UnidadesFactory

class TransporteSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)

  //4. Transportes

  //Cada sucursal posee una cantidad de transportes. De cada transporte se sabe el volumen de carga que puede
  //llevar, su costo por kilometro y su velocidad.
  //Para eso se debe validar:

  //El transporte debe tener suficiente espacio disponible para el volumen del envío en cuestión. Para
  //volumen disponible se debe restar al volumen total el volumen de los pedidos ya asignados a ese transporte.
  "Un transporte" should "poder verificar si puede realizar un envio menor a su capacidad" in {
    var transporte = new Camion

    transporte.puedeTransportarVolumen(40.m3) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad" in {
    var transporte = new Camion

    transporte.puedeTransportarVolumen(150.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 1 carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, 40.m3))

    transporte.puedeTransportarVolumen(10.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 3 carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, 10.m3))
    transporte.agregarEnvio(new Envio(null, null, 10.m3))
    transporte.agregarEnvio(new Envio(null, null, 10.m3))

    transporte.puedeTransportarVolumen(20.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene carga" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, 20.m3))

    transporte.puedeTransportarVolumen(25.m3) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene 2 cargas" in {
    var transporte = new Camion
    transporte.agregarEnvio(new Envio(null, null, 20.m3))
    transporte.agregarEnvio(new Envio(null, null, 20.m3))

    transporte.puedeTransportarVolumen(5.m3) should be(true)
  }

  //Las características del transporte deben ser compatibles con las necesidades del envío, es decir,
  //transporte se debe saber si puede llevar envíos urgentes, frágiles y/o posee refrigeración.
  "Un transporte" should "poder verificar si puede realizar un envio que necesita refrigeracion" in {
    var camion = new Camion
    camion.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(true)

    var avion = new Avion
    avion.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(false)
    
    var furgoneta = new Furgoneta
    furgoneta.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(false)
  }
}