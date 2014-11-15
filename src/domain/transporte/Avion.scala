
package domain.transporte

import domain.Transporte
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import domain.Sucursal
import unidadmedida.Kilometro
import domain.Normal
import domain.CalculadorDistancia
import java.util.Calendar
import domain.Central
import unidadmedida.Dinero

class Avion extends Transporte {

  override def capacidad = VolumenM3(200)
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(500)
  override def velocidad: VelocidadKMH = new VelocidadKMH(500)

  override def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    super.puedeEnviarALaSucursalDestino(sucursalDestino) && distanciaEntre(sucursalActual, sucursalDestino) >= new Kilometro(1000)
  }
  
  override def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    new CalculadorDistancia().distanciaAereaEntre(origen, destino)
  }
  
  
  override def costosExtra(costoDePaquetes: Dinero): Dinero = {
    costoImpuestos(costoDePaquetes) - costoIdaCentralPasadoEl20(costoDePaquetes)
  }
  
  def costoImpuestos(costoDePaquetes: Dinero): Dinero = {
    
    val pais_origen =  this.origen.pais
    val pais_destino = this.destino.pais
    
    if(pais_origen != pais_destino){
      Dinero(costoDePaquetes.value  * 0.1)
    } else return Dinero(0)
    
  }
  
  def costoIdaCentralPasadoEl20(costo:Dinero) = {
    var calendar = Calendar.getInstance();
    calendar.setTime(fechaSalida);
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    
    if(this.destino.equals(Central) && miDia >= 20){
      Dinero(costo.value * 0.2)
    } else Dinero(0)
  }
  
  override def costoVolumenParticular(costoDePaquetes:Dinero):Dinero = {
    Dinero(costoDePaquetes.value * 3)
  }
  
}