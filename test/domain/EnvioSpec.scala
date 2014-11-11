package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory

class EnvioSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)

  "Un cliente" should "poder enviar paquete" in {
    val cliente: Cliente = new Cliente

    val sucursalOrigen: Sucursal = new Sucursal
    val sucursalDestino: Sucursal = new Sucursal
    val volumen: VolumenM3 = 1.m3
    val tipoEnvio: TipoEnvio = Normal

    cliente.enviarPaquete(sucursalOrigen, sucursalDestino, volumen, tipoEnvio)
  }

}