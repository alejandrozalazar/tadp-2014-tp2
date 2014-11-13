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
import domain.Central
import domain.TipoEnvio

class Camion extends Transporte {
  
  override def capacidad = VolumenM3(45)
  override def poseeRefrigeracion: Boolean = true
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
  override def velocidad: VelocidadKMH = new VelocidadKMH(60)
  
  override def tiposEnvioSoportados = super.tiposEnvioSoportados + NecesitaRefrigeracion
  
  override def costoPeajes() = {
    new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 12 // TODO money
  }
  
  override def costosExtra(costoDePaquetes: Double) = {
    this.costoRefrigeracion + this.costoFinDeMes(costoDePaquetes)
  }
  
  def costoRefrigeracion = {
	  5 * cantidadEnviosDelTipo(NecesitaRefrigeracion)
  }
  
  def costoFinDeMes(costo:Double) = {
    if(this.destino.equals(Central) /*&& el temita de la fecha*/){
      costo * 0.02
    } else 0
  }
  
  override def costoVolumenParticular(costoDePaquetes: Double): Double = {
    if(origen.equals(Central) || destino.equals(Central)){
      0
    } else costoDePaquetes * (1 + volumenOcupado.value/capacidad.value)
  }
}