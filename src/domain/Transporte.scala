package domain

import java.util.Date

import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.TransporteNoPoseeInfraestructura
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.Kilometro
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3

abstract class Transporte {

  protected var enviosAsignados: Set[Envio] = Set()
  var poseeGPS: Boolean = false
  var poseeVideo: Boolean = false
  var sucursalActual = Central
  var fechaSalida: Date = new Date
  var infraestructura: Naturaleza = Otro

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
    } else {
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

  def transportaNaturaleza(naturaleza: Naturaleza): Boolean = {
    enviosAsignados.exists(_.naturaleza.equals(naturaleza))
  }

  //  def transportaTipoEnvio(envio: TipoEnvio): Boolean = {
  //    enviosAsignados.exists(_.tipoEnvio.equals(envio))
  //  }

  def agregarEnvio(envio: Envio): Unit = {
    puedeRealizarEnvio(envio)
    enviosAsignados = enviosAsignados + envio
  }

  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    new CalculadorDistancia().distanciaTerrestreEntre(origen, destino)
  }

  def origen() = {
    enviosAsignados.head.sucursalOrigen
  }

  def destino() = {
    enviosAsignados.head.sucursalDestino
  }

  def costoPaquetes(): Dinero = {
    enviosAsignados.foldLeft(Dinero(0)) { (costoTotal, envio) =>
      costoTotal + envio.costo
    }
  }

  def costoDistancia(): Dinero = {
    val distancia = distanciaEntre(origen, destino)
    Dinero(costoPorKilometro.value * distancia.value) // TODO
  }

  def costoPeajes(): Dinero = {
    Dinero(0)
  }

  def costoEnvio(): Dinero = {
    costoDistancia + costoPaquetes + costoPeajes + costosExtra(costoPaquetes) + costoVolumen(costoPaquetes) + costoServiciosExtra + costoInfraestructura
  }

  def costosExtra(costoDePaquetes: Dinero): Dinero = {
    Dinero(0)
  }

  def costoVolumen(costoDePaquetes: Dinero): Dinero = {
    if (volumenOcupado.value <= capacidad.value * 0.2) {
      this.costoVolumenParticular(costoDePaquetes)
    } else Dinero(0)
  }

  def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    Dinero(0)
  }

  def cantidadEnviosDelTipo(tipo: TipoEnvio): Double = {
    this.enviosAsignados.filter((envio: Envio) => envio.tipoEnvio.equals(tipo)).size
  }

  def costoServiciosExtra(): Dinero = {
    var costoExtra = Dinero(0)

    if (poseeGPS) {
      costoExtra += costoGPS
    }
    if (poseeVideo) {
      costoExtra += costoVideo
    }
    costoExtra
  }

  def costoGPS() = {
    Dinero(0.5 * distanciaEntre(origen, destino).value * 2)
  }

  def costoVideo() = {
    Dinero(3.74 * distanciaEntre(origen, destino).value * 2)
  }

  def costoInfraestructura(): Dinero = {
    if (enviosAsignados.exists(_.naturaleza.equals(SustanciaPeligrosa))) {
      Dinero(600)
    } else if (enviosAsignados.exists(_.naturaleza.equals(Animal))) {
      var distancia = distanciaEntre(origen, destino).value
      if (distancia <= 100) Dinero(50)
      else if (distancia <= 200) Dinero(86)
      else Dinero(137)
    } else {
      Dinero(0)
    }
  }

  def gananciaEnvio(): Dinero = {
    val sumatoriaPrecios = enviosAsignados.foldLeft(Dinero(0)) { (costoTotal, envio) => costoTotal + envio.precio }
    sumatoriaPrecios - costoEnvio
  }
}