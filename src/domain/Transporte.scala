package domain

import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import unidadmedida.CostoPorKM
import unidadmedida.CostoPorKM
import unidadmedida.VolumenM3
import exceptions.ValidacionException

class Transporte(var volumenCarga: VolumenM3 = new VolumenM3(0) /*m3*/ ) {

  var costoPorKilometro: CostoPorKM = new CostoPorKM(0)
  var velocidad: VelocidadKMH = new VelocidadKMH(0)
  private var enviosAsignados: Set[Envio] = Set()
  val tipoEnvio: TipoEnvio = Normal

  def puedeRealizarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen))
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio))
    validar(puedeEnviarALaSucursalDestino(envio.sucursalDestino))
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
      volumen.value <= (enviosAsignados.foldLeft(volumenCarga) { (volumenRestante, envio) =>
        new VolumenM3(volumenRestante.value - envio.volumen.value)
      }).value
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
}