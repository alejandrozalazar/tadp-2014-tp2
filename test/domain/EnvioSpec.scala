package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.transporte.Camion
import domain.transporte.Furgoneta
import domain.transporte.Avion
import java.util.Date

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
    
  }
  
  "Los transportes" should "afectar el costo cuando viajan con menos del 20% de su capacidad" in {
    val avion = new Avion
    avion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Normal))
    avion.costoVolumenParticular(100) should be(300)
    
    val furgoneta = new Furgoneta
    furgoneta.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    furgoneta.costoVolumenParticular(100) should be(200)
    
    furgoneta.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    furgoneta.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Urgente))
    furgoneta.costoVolumenParticular(100) should be(0)
    
    val camion = new Camion
    camion.agregarEnvio(new Envio(Central, Mendoza, 1.m3, Normal))
    camion.costoVolumenParticular(100) should be(0)
    
    val camion2 = new Camion
    camion2.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
    camion2.costoVolumenParticular(100) should be(0)
    
    val camion3 = new Camion
    camion3.agregarEnvio(new Envio(Mendoza, BahiaBlanca, 1.m3, Normal))
    camion3.costoVolumenParticular(100) should be(102.22222222222221)
  }
  
  "Los transportes" should "sumar al costo por servicios extra (gps y video)" in {
    val camion = new Camion
    camion.poseeGPS = true
    camion.poseeVideo  = true
    camion.agregarEnvio(new Envio(Mendoza, BahiaBlanca, 1.m3, Normal))
    camion.costoServiciosExtra should be(848)
    
    val avion = new Avion
    avion.poseeGPS  = true
    avion.poseeVideo = true
    avion.agregarEnvio(new Envio(Mendoza, BahiaBlanca, 1.m3, Normal))
    avion.costoServiciosExtra should be(8488.48)
  }
  
  "Los camiones que salen a casa central la ultima semana del mes" should "recargo del dos por ciento del costo" in {
    val camion = new Camion
    val envio = new Envio(Mendoza, Central, 1.m3, Normal)
    camion.agregarEnvio(envio)
    camion.fechaSalida = new Date("12/30/2012")
    camion.costoFinDeMes(100) should be(2)
    camion.fechaSalida = new Date("12/01/2012")
    camion.costoFinDeMes(100) should be(0)
  }
  
  "Los aviones que salen a casa central despues del 20" should "reducir su costo un veinte por ciento" in {
    val avion = new Avion
    val envio = new Envio(Mendoza, Central, 1.m3, Normal)
    avion.agregarEnvio(envio)
    avion.fechaSalida = new Date("12/21/2012")
    avion.costoIdaCentralPasadoEl20(100) should be(-20)
    avion.fechaSalida = new Date("12/19/2012")
    avion.costoIdaCentralPasadoEl20(100) should be(0)
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