package domain.transporte

import domain.Transporte
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH

class Furgoneta extends Transporte {

	override def capacidad = new VolumenM3(9)
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(40)
	override def velocidad: VelocidadKMH = new VelocidadKMH(80)
}