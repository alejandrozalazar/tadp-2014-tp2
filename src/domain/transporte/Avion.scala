
package domain.transporte

import domain.Transporte
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import domain.Sucursal
import domain.Normal
import domain.NecesitaRefrigeracion
import domain.Urgente
import domain.Fragil
import domain.TipoEnvio
import unidadmedida.Kilometro
import domain.Normal
import domain.CalculadorDistancia
import java.util.Calendar
import domain.Central
import unidadmedida.Dinero
import domain.Envio
import java.util.Date


case class Avion(override val sucursal:Sucursal, override val envios:List[Envio], override val tiposDeEnviosSoportados:List[TipoEnvio], override val tieneGps:Boolean, override val tieneSeguimientoSatelital: Boolean, override val fechaSalida:Date) 
			extends Transporte(sucursal, envios, tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital, fechaSalida) {
  
  override val capacidad = 200.m3
  override val velocidad = 500.kmh
  override val costoPorKilometro = 500.pesos
  
  def agregarEnvio(envio:Envio):Avion = {
    validarEnvio(envio)
    envios match {
      case Nil => Avion(sucursal,List(envio),tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital,fechaSalida)
      case xs => {
        validarMismaSucursalEnvios(envio)
        Avion(sucursal,(List(envio)++xs),tiposDeEnviosSoportados,tieneGps, tieneSeguimientoSatelital,fechaSalida)
      }
    } 
  }
  
  /*
  override def capacidad = VolumenM3(200)
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(500)
  override def velocidad: VelocidadKMH = new VelocidadKMH(500)

  override def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    super.puedeEnviarALaSucursalDestino(sucursalDestino) && distanciaEntre(sucursalActual, sucursalDestino) >= 1000.kilometros
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
      costoDePaquetes  * 0.1
    } else return 0.pesos
    
  }
  
  def costoIdaCentralPasadoEl20(costo:Dinero) = {
    var calendar = Calendar.getInstance();
    calendar.setTime(fechaSalida);
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    
    if(this.destino.equals(Central) && miDia >= 20){
      costo * 0.2
    } else 0.pesos
  }
  
  override def costoVolumenParticular(costoDePaquetes:Dinero):Dinero = {
    costoDePaquetes * 3
  }
  */
  
}