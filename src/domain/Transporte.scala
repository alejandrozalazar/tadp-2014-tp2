package domain

import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import unidadmedida.Kilometro
import scala.collection.mutable.HashSet
import java.util.Date
import exceptions.TransporteNoPoseeInfraestructura

abstract class Transporte {

  protected var enviosAsignados: Set[Envio] = Set()
  var poseeGPS:Boolean = false
  var poseeVideo:Boolean = false
  var sucursalActual = Central
  var fechaSalida:Date = new Date
  var infraestructura:Naturaleza = Otro
  
  def tiposEnvioSoportados: Set[TipoEnvio] = Set(Normal, Urgente, Fragil)
  
  def capacidad: VolumenM3 = VolumenM3(0) /*m3*/
  def velocidad: VelocidadKMH = new VelocidadKMH(0)
  def costoPorKilometro: CostoPorKM = new CostoPorKM(0)
  def puedeRealizarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen), TransporteTieneVolumenInsuficienteParaRealizarElEnvio())
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio), TransporteNoSoportaElTipoEnvioEspecificado())
    validar(puedeEnviarALaSucursalDestino(envio.sucursalDestino), TransporteNoSeDirigeALaSucursalDeDestinoEspecificada())
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino), LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible())
    validar(poseeInfraestructura(envio), TransporteNoPoseeInfraestructura())
  }
  
  def poseeInfraestructura(envio: Envio): Boolean = {
    infraestructura.equals(envio.naturaleza) || envio.naturaleza.equals(Otro)
  }

  
  def sucursalDestinoTieneSuficienteEspacio(volumen: VolumenM3, sucursalDestino: Sucursal): Boolean =
    {
      volumen <= sucursalDestino.espacioDisponibleEnSucursal
    }

  def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    if (enviosAsignados.isEmpty) {
      true
    }
    else
    {
      enviosAsignados.iterator.next.sucursalDestino.equals(sucursalDestino)
    }

  }

  def puedeManejarElTipoDeEnvio(tipoEnvioAValidar: TipoEnvio): Boolean = {
    tiposEnvioSoportados.contains(tipoEnvioAValidar)
  }

  def puedeTransportarVolumen(volumen: VolumenM3): Boolean =
    {
      volumen <= (volumenRestante)
    }

  def volumenRestante: VolumenM3 = {
    enviosAsignados.foldLeft(capacidad) { (volumenRestante, envio) =>
        volumenRestante - envio.volumen
      }
  }
  
  def volumenOcupado: VolumenM3 = {
    capacidad - volumenRestante
  }

  def puedeLlevarEnviosUrgentes: Boolean = false
  def puedeLlevarEnviosFragiles: Boolean = false
  def poseeRefrigeracion: Boolean = false

  def validar(resultadoValidacion: Boolean, exception: ValidacionException): Boolean = {
    if (!resultadoValidacion) {
      throw exception
    }
    resultadoValidacion
  }

  def agregarEnvio(envio: Envio): Unit = {
    puedeRealizarEnvio(envio)
    enviosAsignados = enviosAsignados + envio
  }
  
  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    new Kilometro(new CalculadorDistancia().distanciaTerrestreEntre(origen, destino))
  }
  
  def origen() = {
    enviosAsignados.head.sucursalOrigen
  }
  
  def destino() = {
    enviosAsignados.head.sucursalDestino
  }
  
  def costoPaquetes(): Double = {
    enviosAsignados.foldLeft(0.toDouble) { (costoTotal, envio) =>
	    costoTotal + envio.costo
	  }    
  }
  
  def costoDistancia(): Double = {
    val distancia = distanciaEntre(origen, destino)
    costoPorKilometro.value  * distancia.value // TODO
  }
  
  def costoPeajes(): Double = {
    0
  }
  
  def costoEnvio(): Double = {
    costoDistancia + costoPaquetes + costoPeajes + costosExtra(costoPaquetes) + costoVolumen(costoPaquetes) + costoServiciosExtra
  }
  
  def costosExtra(costoDePaquetes: Double): Double = {
    0
  }
  
  def costoVolumen(costoDePaquetes: Double): Double = {
    if(volumenOcupado.value <= capacidad.value * 0.2){
      this.costoVolumenParticular(costoDePaquetes)
    } else 0
  }
  
  def costoVolumenParticular(costoDePaquetes: Double): Double = {
    0
  }
  
  def cantidadEnviosDelTipo(tipo:TipoEnvio): Double = {
    val enviosDelTipo: HashSet[Envio] = new HashSet()
    for(envio <- this.enviosAsignados ){
      if (envio.tipoEnvio.equals(tipo))
      	enviosDelTipo.add(envio)
	}
    enviosDelTipo.size
  }
  
  def costoServiciosExtra(): Double = {
    var costoExtra: Double= 0
    
    if(poseeGPS){
      costoExtra += costoGPS
    }
    if(poseeVideo){
      costoExtra += costoVideo
    }
    costoExtra
  }
  
  def costoGPS() = {
    0.5 * distanciaEntre(origen, destino).value * 2
  }
  
  def costoVideo() = {
    3.74 * distanciaEntre(origen, destino).value * 2
  }
  
  def costoInfraestructura(): Double = {
    if(enviosAsignados.exists(_.naturaleza.equals(SustanciaPeligrosa))) 600
    else 0
  }
}