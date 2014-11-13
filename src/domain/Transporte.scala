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

abstract class Transporte() {

  private var enviosAsignados: Set[Envio] = Set()
  
  var sucursalActual = Central
  
  def tiposEnvioSoportados: Set[TipoEnvio] = Set(Normal, Urgente, Fragil)
  
  def capacidad: VolumenM3 = VolumenM3(0) /*m3*/
  def velocidad: VelocidadKMH = new VelocidadKMH(0)
  def costoPorKilometro: CostoPorKM = new CostoPorKM(0)
  def puedeRealizarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen), TransporteTieneVolumenInsuficienteParaRealizarElEnvio())
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio), TransporteNoSoportaElTipoEnvioEspecificado())
    validar(puedeEnviarALaSucursalDestino(envio.sucursalDestino), TransporteNoSeDirigeALaSucursalDeDestinoEspecificada())
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino), LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible())
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
      volumen <= (enviosAsignados.foldLeft(capacidad) { (volumenRestante, envio) =>
        volumenRestante - envio.volumen
      })
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
  
  def costoEnvio() = {
    val origen = enviosAsignados.head.sucursalOrigen
    val destino = enviosAsignados.head.sucursalDestino
    val distancia = distanciaEntre(origen, destino)
    val costoTransporte = costoPorKilometro.value  * distancia.value // TODO
    
    enviosAsignados.foldLeft(costoTransporte.toDouble) { (costoTotal, envio) =>
	    costoTotal + envio.costo
	  }
  }
}