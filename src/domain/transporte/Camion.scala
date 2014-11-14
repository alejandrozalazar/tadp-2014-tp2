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
import domain.CalculadorDistancia
import scala.collection.mutable.HashSet
import domain.Envio
import domain.Central
import domain.TipoEnvio
import java.util.Date
import java.util.Calendar
import domain.SustanciaPeligrosa

class Camion extends Transporte {
  
  override def capacidad = VolumenM3(45)
  override def poseeRefrigeracion: Boolean = true
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
  override def velocidad: VelocidadKMH = new VelocidadKMH(60)
  
  override def tiposEnvioSoportados = super.tiposEnvioSoportados + NecesitaRefrigeracion
  
  override def costoPeajes() = {
    new CalculadorDistancia().cantidadPeajesEntre(origen, destino) * 12 // TODO money
  }
  
  override def costosExtra(costoDePaquetes: Double) = {
    this.costoRefrigeracion + this.costoFinDeMes(costoDePaquetes)
  }
  
  def costoRefrigeracion = {
	  5 * cantidadEnviosDelTipo(NecesitaRefrigeracion)
  }
  
  def costoFinDeMes(costo:Double) = {
    var calendar = Calendar.getInstance();  
    calendar.setTime(fechaSalida);
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    calendar.add(Calendar.MONTH, 1);  
    calendar.set(Calendar.DAY_OF_MONTH, 1);  
    calendar.add(Calendar.DATE, -1);
    var ultimoDia = calendar.get(Calendar.DAY_OF_MONTH);
    
    var diferencia = ultimoDia - miDia;
    var estaEnLaUltimaSemana = (diferencia <= 7);
    
    if(this.destino.equals(Central) && estaEnLaUltimaSemana){
      costo * 0.02
    } else 0
  }
  
  def costoSustanciasPeligrosasUrgentes(): Double = {
    var peligrosos = enviosAsignados.filter(_.naturaleza.equals(SustanciaPeligrosa))
    var urgentes = enviosAsignados.filter(_.tipoEnvio.equals(Urgente))
    if(peligrosos.isEmpty || urgentes.isEmpty) return 0
    var volumenPaquetes = urgentes.foldLeft(0.toDouble) { (volumen, envio) =>
        volumen + envio.volumen.value
      }
    3 * volumenPaquetes / capacidad.value
  }
  
  override def costoVolumenParticular(costoDePaquetes: Double): Double = {
    if(origen.equals(Central) || destino.equals(Central)){
      0
    } else costoDePaquetes * (1 + volumenOcupado.value/capacidad.value)
  }
}