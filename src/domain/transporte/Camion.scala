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
import unidadmedida.Dinero

class Camion extends Transporte {

  override def capacidad = VolumenM3(45)
  override def poseeRefrigeracion: Boolean = true
  override def costoPorKilometro: CostoPorKM = new CostoPorKM(100)
  override def velocidad: VelocidadKMH = new VelocidadKMH(60)

  override def tiposEnvioSoportados = super.tiposEnvioSoportados + NecesitaRefrigeracion

  override def costoPeajes() = {
    Dinero(cantidadPeajesEntre(origen, destino) * 12)
  }

  override def costosExtra(costoDePaquetes: Dinero) = {
    costoRefrigeracion + costoFinDeMes(costoDePaquetes) + costoSustanciasPeligrosasUrgentes
  }

  def costoRefrigeracion = {
    Dinero(5 * cantidadEnviosDelTipo(NecesitaRefrigeracion))
  }

  def costoFinDeMes(costo: Dinero) = {
    var calendar = Calendar.getInstance();
    calendar.set(fechaSalida.get(Calendar.YEAR), fechaSalida.get(Calendar.MONTH), fechaSalida.get(Calendar.DAY_OF_MONTH));
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    calendar.add(Calendar.MONTH, 1);
    calendar.set(Calendar.DAY_OF_MONTH, 1);
    calendar.add(Calendar.DATE, -1);
    var ultimoDia = calendar.get(Calendar.DAY_OF_MONTH);

    var diferencia = ultimoDia - miDia;
    var estaEnLaUltimaSemana = (diferencia <= 7);

    if (this.destino.equals(Central) && estaEnLaUltimaSemana) {
      costo * 0.02
    } else 0.pesos
  }

  def costoSustanciasPeligrosasUrgentes(): Dinero = {
    if (!transportaNaturaleza(SustanciaPeligrosa)) return 0.pesos

    val foldeado: (Set[Envio] => Double) = _.foldLeft(0.toDouble) { (volumen, envio) => volumen + envio.volumen.value }
    val filtrado: (Set[Envio] => Set[Envio]) = _.filter(_.tipoEnvio.equals(Urgente))
    var volumenPaquetes = (foldeado compose filtrado)(enviosAsignados)
    Dinero(3 * volumenPaquetes / capacidad.value)
  }

  override def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    if (origen.equals(Central) || destino.equals(Central)) {
      0.pesos
    } else costoDePaquetes * (1 + volumenOcupado.value / capacidad.value)
  }
}