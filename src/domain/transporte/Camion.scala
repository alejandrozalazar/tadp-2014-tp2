package domain.transporte

import domain.Transporte
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VelocidadKMH

class Camion extends Transporte {

	override def capacidad = new VolumenM3(45)
	override def poseeRefrigeracion: Boolean = true
	override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
	override def velocidad: VelocidadKMH = new VelocidadKMH(60)
}