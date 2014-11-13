package domain.transporte

import domain.Transporte
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VelocidadKMH
import domain.Normal
import domain.NecesitaRefrigeracion
import domain.Urgente
import domain.Fragil
import domain.CalculadorDistancia
import scala.collection.mutable.HashSet
import domain.Envio

class Camion extends Transporte {
  
  override def capacidad = VolumenM3(45)
  override def poseeRefrigeracion: Boolean = true
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
  override def velocidad: VelocidadKMH = new VelocidadKMH(60)
  
  override def tiposEnvioSoportados = super.tiposEnvioSoportados + NecesitaRefrigeracion
  
  override def costoPeajes() = {
    new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 12 // TODO money
  }
  
  override def costosExtra() = {
    this.costoRefrigeracion
  }
  
  def costoRefrigeracion = {
	  
	  val enviosConRefrigeracion: HashSet[Envio] = new HashSet()
	  for(envio <- this.enviosAsignados ){
	    if (envio.tipoEnvio.equals(NecesitaRefrigeracion))
	    	enviosConRefrigeracion.add(envio)
	  }
	  
	  5 * enviosConRefrigeracion.size
  }
}