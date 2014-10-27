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
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino))
  }

  def sucursalDestinoTieneSuficienteEspacio(volumen: VolumenM3, sucursalDestino: Sucursal): Boolean =
    {
      val enviosAcumuladosEnSucursal = sucursalDestino.enviosAcumulados

      val espacioDisponibleMenosEnviosAcumulados = (enviosAcumuladosEnSucursal.foldLeft(sucursalDestino.volumenDepositoSucursal) { (volumenRestante, envio) =>
        volumenRestante - envio.volumen
      })

      val enviosLlegandoASucursal = sucursalDestino.enviosLlegandoASucursal

      val espacioDisponibleEnSucursal = (enviosLlegandoASucursal.foldLeft(espacioDisponibleMenosEnviosAcumulados) { (volumenRestante, envio) =>
        volumenRestante - envio.volumen
      })

      volumen <= espacioDisponibleEnSucursal
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
      volumen <= (enviosAsignados.foldLeft(volumenCarga) { (volumenRestante, envio) =>
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
}