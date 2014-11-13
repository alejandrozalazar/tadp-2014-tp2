package domain.transporte

import domain.Transporte
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import domain.Normal
import domain.CalculadorDistancia
import domain.Urgente

class Furgoneta extends Transporte {

	override def capacidad = VolumenM3(9)
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(40)
	override def velocidad: VelocidadKMH = new VelocidadKMH(80)
	
  override def costoPeajes() = {
    new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 6 // TODO money
  }

  override def costoVolumenParticular(costoDePaquetes: Double): Double = {
    if(cantidadEnviosDelTipo(Urgente) < 3){
      costoDePaquetes * 2
    } else 0
  }
}