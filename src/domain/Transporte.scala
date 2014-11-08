package domain

import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.CostoPorKM
import unidadmedida.VolumenM3
import exceptions.ValidacionException

/*abstract */class Transporte() {

  private var enviosAsignados: Set[Envio] = Set()
  val tipoEnvio: TipoEnvio = Normal

  def capacidad: VolumenM3 = new VolumenM3(0) /*m3*/
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
    tipoEnvio.equals(tipoEnvioAValidar)
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