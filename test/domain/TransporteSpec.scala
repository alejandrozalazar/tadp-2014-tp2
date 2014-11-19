package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import domain.transporte.Avion
import domain.transporte.Camion
import domain.transporte.Furgoneta
import domain.Sucursal
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
    
   val camion = Camion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)

   camion.capacidad	should be(45.m3)
  }
  
  "Un camion" should "aguanta dos envios de 10 m3" in {
    
    var camion = Camion(Central,Nil, List(Urgente, Normal, Fragil),false,false,null)
    
    val envio = Envio(Rio,10.m3,Urgente,null)
    
    camion = camion.agregarEnvio(envio)
    
    camion.envios should be(List(envio))
    
    camion = camion.agregarEnvio(envio)
    
    camion.envios should be(List(envio,envio))
    
  }
  
   "Un camion" should "no aguanta 2 envios de 30 m3" in {
    
    var camion = Camion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
    
    val envio = Envio(null,30.m3,Urgente,null)
    
    camion = camion.agregarEnvio(envio)
    //camion = camion.agregarEnvio(envio)  
    
    intercept[TransporteTieneVolumenInsuficienteParaRealizarElEnvio] {
    	camion = camion.agregarEnvio(envio) 
    }

  }
   
   "Un Avion" should "no puede transportar un envio que necesita refrigeracion" in {
    
    var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
    
    val envio = Envio(null,30.m3,NecesitaRefrigeracion,null)
    
   
    intercept[TransporteNoSoportaElTipoEnvioEspecificado] {
    	avion = avion.agregarEnvio(envio) 
    }

  }
  
   "Un transporte" should "no puede llevar envios de dos sucursales distintas" in {
    
    var avion = Avion(Central,Nil,List(Urgente, Normal, Fragil),false,false,null)
    
    val envio1 = Envio(Mendoza,30.m3,Normal,null)
    val envio2 = Envio(Rio,30.m3,Normal,null)
    
    avion = avion.agregarEnvio(envio1)
    
    intercept[TransporteNoSeDirigeALaSucursalDeDestinoEspecificada] {
    	avion = avion.agregarEnvio(envio2) 
    }

  }
   
}