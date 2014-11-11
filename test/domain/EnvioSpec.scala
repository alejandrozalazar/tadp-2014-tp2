package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3

class EnvioSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)

  "Un cliente" should "poder enviar paquete" in {
    val cliente: Cliente = new Cliente

    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = BahiaBlanca 
    val volumen: VolumenM3 = 1.m3
    val tipoEnvio: TipoEnvio = Normal

    cliente.enviarPaquete(sucursalOrigen, sucursalDestino, volumen, tipoEnvio)
  }

}