
package domain.transporte

import domain.Transporte
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import domain.Sucursal
import unidadmedida.Kilometro
import domain.Normal
import domain.CalculadorDistancia

class Avion extends Transporte {

  override def capacidad = VolumenM3(200)
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(500)
  override def velocidad: VelocidadKMH = new VelocidadKMH(500)

  override def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    super.puedeEnviarALaSucursalDestino(sucursalDestino) && distanciaEntre(sucursalActual, sucursalDestino) >= new Kilometro(1000)
  }
  
  override def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    new Kilometro(new CalculadorDistancia().distanciaAereaEntre(origen, destino))
  }
  
}