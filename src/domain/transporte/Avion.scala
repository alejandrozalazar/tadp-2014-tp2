
package domain.transporte

import domain.Transporte
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import domain.Sucursal
import unidadmedida.Kilometro

class Avion extends Transporte {

  override def capacidad = new VolumenM3(200)
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(500)
  override def velocidad: VelocidadKMH = new VelocidadKMH(500)

  override def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    super.puedeEnviarALaSucursalDestino(sucursalDestino) && sucursalActual.distanciaASucursal(sucursalDestino) >= new Kilometro(1000)
  }
}