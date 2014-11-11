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

class Camion extends Transporte {
  
  override def capacidad = VolumenM3(45)
  override def poseeRefrigeracion: Boolean = true
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
  override def velocidad: VelocidadKMH = new VelocidadKMH(60)
  
  override def tiposEnvioSoportados = super.tiposEnvioSoportados + NecesitaRefrigeracion
}