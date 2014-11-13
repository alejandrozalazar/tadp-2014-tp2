package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import domain.transporte.Avion
import domain.transporte.Camion
import domain.transporte.Furgoneta
import unidadmedida.UnidadesFactory
import exceptions.ValidacionException
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import unidadmedida.VolumenM3

class TransporteSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
  
  case object transporteMock extends Transporte(){
    override def capacidad = VolumenM3(1000)
  }

  //4. Transportes

  //Cada sucursal posee una cantidad de transportes. De cada transporte se sabe el volumen de carga que puede
  //llevar, su costo por kilometro y su velocidad.
  //Para eso se debe validar:

  //El transporte debe tener suficiente espacio disponible para el volumen del envío en cuestión. Para
  //volumen disponible se debe restar al volumen total el volumen de los pedidos ya asignados a ese transporte.
  "Un transporte" should "poder verificar si puede realizar un envio menor a su capacidad" in {
    val transporte = new Camion

    transporte.puedeTransportarVolumen(40.m3) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad" in {
    val transporte = new Camion

    transporte.puedeTransportarVolumen(150.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 1 carga" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 40.m3))

    transporte.puedeTransportarVolumen(10.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio mayor a su capacidad si ya tiene 3 carga" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 10.m3))
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 10.m3))
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 10.m3))

    transporte.puedeTransportarVolumen(20.m3) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene carga" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 20.m3))

    transporte.puedeTransportarVolumen(25.m3) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio igual a su capacidad si ya tiene 2 cargas" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 20.m3))
    transporte.agregarEnvio(new Envio(Central, BahiaBlanca, 20.m3))

    transporte.puedeTransportarVolumen(5.m3) should be(true)
  }

  //Las características del transporte deben ser compatibles con las necesidades del envío, es decir,
  //transporte se debe saber si puede llevar envíos urgentes, frágiles y/o posee refrigeración.

  "Un transporte" should "poder verificar si puede realizar un envio normal" in {
    val camion = new Camion
    camion.puedeManejarElTipoDeEnvio(Normal) should be(true)

    val avion = new Avion
    avion.puedeManejarElTipoDeEnvio(Normal) should be(true)

    val furgoneta = new Furgoneta
    furgoneta.puedeManejarElTipoDeEnvio(Normal) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio que necesita refrigeracion" in {
    val camion = new Camion
    camion.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(true)

    val avion = new Avion
    avion.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(false)

    val furgoneta = new Furgoneta
    furgoneta.puedeManejarElTipoDeEnvio(NecesitaRefrigeracion) should be(false)
  }

  "Un transporte" should "poder verificar si puede realizar un envio urgente" in {
    val camion = new Camion
    camion.puedeManejarElTipoDeEnvio(Urgente) should be(true)

    val avion = new Avion
    avion.puedeManejarElTipoDeEnvio(Urgente) should be(true)

    val furgoneta = new Furgoneta
    furgoneta.puedeManejarElTipoDeEnvio(Urgente) should be(true)
  }

  "Un transporte" should "poder verificar si puede realizar un envio fragil" in {
    val camion = new Camion
    camion.puedeManejarElTipoDeEnvio(Fragil) should be(true)

    val avion = new Avion
    avion.puedeManejarElTipoDeEnvio(Fragil) should be(true)

    val furgoneta = new Furgoneta
    furgoneta.puedeManejarElTipoDeEnvio(Fragil) should be(true)
  }

  //Todos los destinos de sus envíos deben tener la misma sucursal destino (es decir, un transporte va a una
  //sucursal destino). Cuando el camión está vacío puede aceptar pedido para cualquier destino, una vez que se
  //le asignó el primer envío todos los demás pedidos que se asignen deben ir

  "Un transporte" should "poder contener dos envios hacia la misma sucursal" in {

    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = BahiaBlanca

    val transporte = new Camion
    transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 20.m3))
    transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 20.m3))
  }

  "Un transporte" should "no contener dos envios hacia distintas sucursales" in {

    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = Mendoza
    val sucursalDestino2: Sucursal = BahiaBlanca

    val transporte = new Camion
    transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 20.m3))
    intercept[TransporteNoSeDirigeALaSucursalDeDestinoEspecificada] {
		transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino2, 20.m3))
	}
  }
  
  "Un transporte" should "no contener dos envios hacia distintas sucursales con 2 envios correctos" in {

    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = Mendoza
    val sucursalDestino2: Sucursal = BahiaBlanca

    val transporte = new Camion
    transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 10.m3))
    transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 10.m3))
    intercept[TransporteNoSeDirigeALaSucursalDeDestinoEspecificada] {
		transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino2, 20.m3))
	}
  }
  
//  La sucursal de destino debe poseer espacio f ́ısico para poder recibir el env ́ıo. De cada sucursal se
//volumen de su dep ́osito. El espacio disponible ser ́a el volumen total menos el volumen de los env
//en la sucursal esperando para partir y menos los env ́ıos que est ́an viajando hacia la sucursal.
  
  "La sucursal destino" should "poseer espacio fisico para recibir un envio" in {
    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = Mendoza
    val transporte = transporteMock
    intercept[LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible]{
	  transporte.agregarEnvio(new Envio(sucursalOrigen, sucursalDestino, 600.m3))      
    }
  }
  
}