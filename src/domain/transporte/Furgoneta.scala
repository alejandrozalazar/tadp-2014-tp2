package domain.transporte

import domain.CalculadorDistancia
import domain.Transporte
import domain.Urgente
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3

class Furgoneta extends Transporte {

	override def capacidad = VolumenM3(9)
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(40)
	override def velocidad: VelocidadKMH = new VelocidadKMH(80)
	
  override def costoPeajes() = {
    Dinero(new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 6)
  }

  override def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    if(cantidadEnviosDelTipo(Urgente) < 3){
      Dinero(costoDePaquetes.value * 2)
    } else Dinero(0)
  }
}