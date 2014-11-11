package domain

import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3

abstract class Transporte() {

  private var enviosAsignados: Set[Envio] = Set()
  
  def tiposEnvioSoportados: Set[TipoEnvio]
  
  def capacidad: VolumenM3 = VolumenM3(0) /*m3*/
  def velocidad: VelocidadKMH = new VelocidadKMH(0)
  def costoPorKilometro: CostoPorKM = new CostoPorKM(0)
  def puedeRealizarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen))
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio))
    validar(puedeEnviarALaSucursalDestino(envio.sucursalDestino))
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino))
  }

  
  def sucursalDestinoTieneSuficienteEspacio(volumen: VolumenM3, sucursalDestino: Sucursal): Boolean =
    {
      volumen <= sucursalDestino.espacioDisponibleEnSucursal
    }

  def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    if (enviosAsignados.isEmpty) {
      true
    }

    enviosAsignados.iterator.next.sucursalDestino.equals(sucursalDestino)
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

  def validar(resultadoValidacion: Boolean): Boolean = {
    if (!resultadoValidacion) {
      throw new ValidacionException
    }
    resultadoValidacion
  }

  def agregarEnvio(envio: Envio): Unit = {
    enviosAsignados = enviosAsignados + envio
  }
  
  def sucursalActual = new Sucursal
}