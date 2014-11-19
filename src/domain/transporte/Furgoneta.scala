package domain.transporte

import domain.CalculadorDistancia
import domain.Transporte
import domain.Urgente
import domain.Sucursal
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import domain.Envio
import domain.Normal
import domain.NecesitaRefrigeracion
import domain.Urgente
import domain.Fragil
import domain.TipoEnvio
import java.util.Date


case class Furgoneta(override val sucursal:Sucursal, override val envios:List[Envio], override val tiposDeEnviosSoportados:List[TipoEnvio], override val tieneGps:Boolean, override val tieneSeguimientoSatelital: Boolean, override val fechaSalida:Date) 
			extends Transporte(sucursal, envios, tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital,fechaSalida) {
  
    override val capacidad = 9.m3
    override val velocidad = 80.kmh
    override val costoPorKilometro = 40.pesos
    
    def agregarEnvio(envio:Envio):Furgoneta = {
    validarEnvio(envio)
    envios match {
      case Nil => Furgoneta(sucursal,List(envio),tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital,fechaSalida)
      case xs => {
        validarMismaSucursalEnvios(envio)
        Furgoneta(sucursal,(List(envio)++xs),tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital,fechaSalida)
      }
    } 
  }
  
    override def costoPeajes() = {
      Dinero(new CalculadorDistancia().cantidadPeajesEntre(sucursal, destino) * 6)
  }
  /*
	override def capacidad = VolumenM3(9)
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(40)
	override def velocidad: VelocidadKMH = new VelocidadKMH(80)
	
 

  override def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    if(cantidadEnviosDelTipo(Urgente) < 3){
      costoDePaquetes * 2
    } else 0.pesos
  }
  * 
  */
}