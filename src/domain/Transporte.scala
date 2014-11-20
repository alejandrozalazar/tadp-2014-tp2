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
import unidadmedida.UnidadesFactory
import domain.estadisticas.Estadisticas

abstract class Transporte {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  var viajeAsignado: Viaje = new Viaje(Central, Mendoza, this)
  var poseeGPS: Boolean = false
  var poseeVideo: Boolean = false
  var infraestructura: Naturaleza = Otro

  def enviosAsignados() = {
    viajeAsignado.envios
  }
  def fechaSalida() = {
    viajeAsignado.fechaSalida
  }
  def sucursalActual() = {
    viajeAsignado.sucursalOrigen
  }
  def setFechaSalida(fecha: Date) = {
    viajeAsignado.fechaSalida = fecha
  }

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
  
  def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    if (!viajeAsignado.tieneEnvios) {
      true
    } else {
      viajeAsignado.sucursalDestino.equals(sucursalDestino)
    }
  }

  def sucursalDestinoTieneSuficienteEspacio(volumen: VolumenM3, sucursalDestino: Sucursal): Boolean =
    {
      volumen <= sucursalDestino.espacioDisponibleEnSucursal
    }

  def puedeManejarElTipoDeEnvio(tipoEnvioAValidar: TipoEnvio): Boolean = {
    tiposEnvioSoportados.contains(tipoEnvioAValidar)
  }

  def puedeTransportarVolumen(volumen: VolumenM3): Boolean =
    {
      volumen <= (volumenRestante)
    }

  def volumenRestante: VolumenM3 = {
    capacidad - viajeAsignado.volumenOcupado
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

  def agregarEnvio(envio: Envio): Unit = {
    if(!viajeAsignado.tieneEnvios){
      viajeAsignado.sucursalOrigen = envio.sucursalOrigen
      viajeAsignado.sucursalDestino = envio.sucursalDestino
    }
    puedeRealizarEnvio(envio)
    viajeAsignado.agregarEnvio(envio)
  }

  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    new CalculadorDistancia().distanciaTerrestreEntre(origen, destino)
  }

  def origen() = {
    viajeAsignado.sucursalOrigen
  }

  def destino() = {
    viajeAsignado.sucursalDestino
  }

  def costoDistancia(): Dinero = {
    val distancia = distanciaEntre(origen, destino)
    Dinero(costoPorKilometro.value * distancia.value) // TODO
  }

  def costoPeajes(): Dinero = {
    0.pesos
  }
  
  def costoPaquetes() = viajeAsignado.costoPaquetes

  def costoEnvio(): Dinero = {
    costoDistancia + costoPaquetes + costoPeajes + costosExtra(costoPaquetes) + costoVolumen(costoPaquetes) + costoServiciosExtra + costoInfraestructura
  }

  def costosExtra(costoDePaquetes: Dinero): Dinero = {
    0.pesos
  }

  def costoVolumen(costoDePaquetes: Dinero): Dinero = {
    if (volumenOcupado.value <= capacidad.value * 0.2) {
      this.costoVolumenParticular(costoDePaquetes)
    } else 0.pesos
  }

  def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    0.pesos
  }

  def cantidadEnviosDelTipo(tipo: TipoEnvio): Double = {
    this.enviosAsignados.filter((envio: Envio) => envio.tipoEnvio.equals(tipo)).size
  }

  def costoServiciosExtra(): Dinero = {
    var costoExtra = 0.pesos

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

  def costoVideo():Dinero = {
    Dinero(3.74 * distanciaEntre(origen, destino).value * 2)
  }

  def costoInfraestructura(): Dinero = {
    if (enviosAsignados.exists(_.naturaleza.equals(SustanciaPeligrosa))) {
      600.pesos
    } else if (enviosAsignados.exists(_.naturaleza.equals(Animal))) {
      var distancia = distanciaEntre(origen, destino).value
      if (distancia <= 100) 50.pesos
      else if (distancia <= 200) 86.pesos
      else 137.pesos
    } else {
      0.pesos
    }
  }

  def gananciaEnvio(): Dinero = {
    val sumatoriaPrecios = enviosAsignados.foldLeft(0.pesos) { (costoTotal, envio) => costoTotal + envio.precio }
    sumatoriaPrecios - costoEnvio
  }
  
  def realizarViaje() = {
    Estadisticas.agregarViajeRealizado(viajeAsignado)
    viajeAsignado.costoFacturado = costoEnvio
    viajeAsignado = new Viaje(Central, Mendoza, this)//Medio feo, hay que inicializarlo si o si
  }
}