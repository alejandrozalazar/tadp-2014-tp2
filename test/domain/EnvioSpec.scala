package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.transporte.Camion
import domain.transporte.Furgoneta
import domain.transporte.Avion

class EnvioSpec extends FlatSpec with Matchers {

  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
  
  "El costo de un transporte" should "ser el costo del paquete que transporta" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Normal))
    transporte.costoPaquetes() should be(10)
  }
  
  "El costo de un transporte" should "ser la suma de los costos de sus paquetes" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Normal))
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Urgente))
    transporte.costoPaquetes() should be(30)
  }
  
  "El costo de un transporte" should "ser el costo por la distancia recorrida" in {
    val transporte = new Camion
    transporte.agregarEnvio(new Envio(Central, Mendoza, 10.m3, Normal))
    transporte.costoDistancia() should be(10000)
  }
  
  "El costo de un transporte" should "ser el costo de los peajes atravesados" in {
    val envio = new Envio(Central, Mendoza, 1.m3, Normal)
    val camion = new Camion
    camion.agregarEnvio(envio)
    camion.costoPeajes() should be(24)
    val furgoneta = new Furgoneta
    furgoneta.agregarEnvio(envio)
    furgoneta.costoPeajes() should be(12)
    val avion = new Avion
    avion.agregarEnvio(envio)
    avion.costoPeajes() should be(0)
  }
  
  "Los paquetes que necesitan refrigeracion" should "agregar $5 al costo" in {
    val envio = new Envio(Central, Mendoza, 1.m3, NecesitaRefrigeracion)
    val camion = new Camion
    camion.agregarEnvio(envio)
    camion.costoRefrigeracion should be(5)
  }
  
  "Los aviones" should "pagar 10% de impuestos cuando van de un pais a otro" in {
    val envio = new Envio(Central, Mendoza, 1.m3, Normal)
    val avion = new Avion
    avion.agregarEnvio(envio)
    avion.costoImpuestos(10) should be(0)
    
    val avion2 = new Avion
    avion2.agregarEnvio(new Envio(Central, Rio, 1.m3, Normal))
    avion2.costoImpuestos(10) should be(1)
    
    val avion3 = new Avion
    avion3.agregarEnvio(new Envio(Central, Rio, 1.m3, Normal))
    avion3.costoEnvio should be(500511)
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