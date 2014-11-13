package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.transporte.Camion

class EnvioSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
  
  "El costo de un transporte" should "ser el costo del paquete que transporta" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Normal))
    transporte.costoEnvio() should be(10010)
  }
  
  "El costo de un transporte" should "ser la suma de los costos de sus paquetes" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Normal))
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Urgente))
    transporte.costoEnvio() should be(10030)
  }

  "Un cliente" should "poder enviar paquete" in {
    val cliente: Cliente = new Cliente

    val sucursalOrigen: Sucursal = Central
    val sucursalDestino: Sucursal = BahiaBlanca 
    val volumen: VolumenM3 = 1.m3
    val tipoEnvio: TipoEnvio = Normal

    cliente.enviarPaquete(sucursalOrigen, sucursalDestino, volumen, tipoEnvio)
  }

}