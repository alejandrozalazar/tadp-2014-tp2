package domain.transporte

import domain.Transporte
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import domain.Normal
import domain.CalculadorDistancia

class Furgoneta extends Transporte {

	override def capacidad = VolumenM3(9)
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(40)
	override def velocidad: VelocidadKMH = new VelocidadKMH(80)
	
  override def costoPeajes() = {
    new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 6 // TODO money
  }
}