package domain

import java.util.GregorianCalendar

import domain.estadisticas.Estadisticas
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.TransporteNoPoseeInfraestructura
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.Kilometro
import unidadmedida.UnidadesFactory
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3

// Ninguna clase de mi dominio puede implementar el "CalculadorDistancia", porque por definición
//  es algo que nosotros NO tenemos que hacer / que alguien hace por nosotros.
//
// En vez de abstract class podría ser un mixin "trait" de scala si no necesitan parámetros de construcción
abstract class Transporte extends CalculadorDistancia{

  // NO! esto es codigo de producción, no de testing, el transporte no puede tener
  //  asignado el viaje!
  var viajeAsignado: Viaje = new Viaje(Central, Mendoza, this)
  // Dejar que estos valores sean abstractos, no definirlos y overridearlos luego (eso esconde que algo es obligatorio).
  // Esta estructura permite que ambos estén definidos a la vez (eso es inválido), entonces o lo validan o lo limitan usando un solo atributo
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
  def setFechaSalida(fecha: GregorianCalendar) = {
    viajeAsignado = viajeAsignado.copy(fechaSalida = fecha)
  }

  def tiposEnvioSoportados: Set[TipoEnvio] = Set(Normal, Urgente, Fragil)

  // NO definir valores "default" y luego pisarlos, preferir valores abstractos!
  def capacidad: VolumenM3 = VolumenM3(0) /*m3*/
  def velocidad: VelocidadKMH = new VelocidadKMH(0)
  def costoPorKilometro: CostoPorKM = new CostoPorKM(0)
  def puedeRealizarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen), TransporteTieneVolumenInsuficienteParaRealizarElEnvio)
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio), TransporteNoSoportaElTipoEnvioEspecificado())
    validar(puedeEnviarALaSucursalDestino(envio.sucursalDestino), TransporteNoSeDirigeALaSucursalDeDestinoEspecificada())
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino), LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible())
    validar(poseeInfraestructura(envio), TransporteNoPoseeInfraestructura())
  }

  def poseeInfraestructura(envio: Envio): Boolean = {
    // no entiendo el "otro"... si necesitan que algo pueda no estar revisen el tipo "Option"
    infraestructura.equals(envio.naturaleza) || envio.naturaleza.equals(Otro)
  }

  def puedeEnviarALaSucursalDestino(sucursalDestino: Sucursal): Boolean = {
    if (!viajeAsignado.tieneEnvios) {
      true
    } else {
      viajeAsignado.sucursalDestino.equals(sucursalDestino)
    }
    // más cortito: !viajeAsignado.tieneEnvios || viajeAsignado.sucursalDestino == sucursalDestino
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

  // Si siempre es false para que sirve??? Nadie usa estos valores?
  def puedeLlevarEnviosUrgentes: Boolean = false
  def puedeLlevarEnviosFragiles: Boolean = false
  def poseeRefrigeracion: Boolean = false

  // Esto es un error de concepto importante!
  //   Una funcion (en terminos de funcional puro) no puede NO retornar nada, por lo que las
  //   excepciones no existen. Cuando hay excepciones, retornar es algo que puede o no pasar.
  // Cuando se aplican validaciones hay dos alternativas:
  //   - retorno un boolean si pasa o no, y luego el cliente decide como manejar el dato (filtrarlo, descartarlo, etc).
  //   - tiro una exceción si no cumple la condición o NO retorno nada si la cumple
  // Si retorno un boolean estoy esperando 2 valores, si tiro la excepción el valor de retorno no sirve de nada!
  def validar(resultadoValidacion: Boolean, exception: ValidacionException): Boolean = {
    // detalle de scala, si "resultadoValidacion" se define como "resultadoValidacion: => Boolean"
    //   la evaluación es lazy
    if (!resultadoValidacion) {
      throw exception
    }
    resultadoValidacion
  }

  def transportaNaturaleza(naturaleza: Naturaleza): Boolean = {
    enviosAsignados.exists(_.naturaleza.equals(naturaleza))
  }

  def agregarEnvio(envio: Envio): Unit = {
    if (!viajeAsignado.tieneEnvios) {
      viajeAsignado= viajeAsignado .copy(sucursalOrigen = envio.sucursalOrigen, sucursalDestino = envio.sucursalDestino)
    }
    puedeRealizarEnvio(envio)
    viajeAsignado = viajeAsignado.agregarEnvio(envio)
  }

  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    distanciaTerrestreEntre(origen, destino)
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
    // y si todos estos "costos" fueran objetos en una colección donde los pueda sumar?
    // y si esa colección dependiera del transporte?
    // (se ve que poner uno abajo del otro solo lleva a un código muy dificil de mantener y mucho más acoplado?)
    val costoDist = costoDistancia
    val costoPaq = costoPaquetes
    val costoPeaj = costoPeajes
    val costosExt = costosExtra(costoPaquetes)
    val costoVol = costoVolumen(costoPaquetes)
    val costoServ = costoServiciosExtra
    val costoInf = costoInfraestructura

    costoDistancia + costoPaquetes + costoPeajes + costosExtra(costoPaquetes) + costoVolumen(costoPaquetes) + costoServiciosExtra + costoInfraestructura
  }

  // si algo es abstracto no lo definan! (similar a lo que pasa con los valores de más arriba)
  def costosExtra(costoDePaquetes: Dinero): Dinero = {
    0.pesos
  }

  def costoVolumen(costoDePaquetes: Dinero): Dinero = {
    if (volumenOcupado.value <= capacidad.value * 0.2) {
      this.costoVolumenParticular(costoDePaquetes)
    } else 0.pesos
  }

  // igual que antes, si algo no se puede definir es abstracto (no usar valores default)
  def costoVolumenParticular(costoDePaquetes: Dinero): Dinero = {
    0.pesos
  }

  def cantidadEnviosDelTipo(tipo: TipoEnvio): Double = {
    this.enviosAsignados.filter((envio: Envio) => envio.tipoEnvio.equals(tipo)).size
  }

  def costoServiciosExtra(): Dinero = {
    var costoExtra = 0.pesos
    // podrían generalizar estos dos en un tipo polimórfico o con funciones

    if (poseeGPS) {
      costoExtra += costoGPS
    }
    if (poseeVideo) {
      costoExtra += costoVideo
    }
    costoExtra
  }

  // podrían generalizar estos costos e integrarlos con un tipo polimórfico o con funciones
  def costoGPS() = {
    Dinero(0.5 * distanciaEntre(origen, destino).value * 2)
  }

  def costoVideo(): Dinero = {
    Dinero(3.74 * distanciaEntre(origen, destino).value * 2)
  }

  def costoInfraestructura(): Dinero = {
	  // Cuidado! el transporte es el que está preparado para sustancias peligrosas o animales (y no ambas a la vez).
    // Esto no funciona como el destino (donde el primer paquete setea como va a ser), depende
    //   de la instancia particular del transporte
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

  // Los transportes tienen que registrar (por separado) cuando salen, cuando llegan a destino, y cuando vuelven a origen
  // Es más, en su caso parecería que este realizar viaje tiene más que ver con el "Viaje" que con el transporte en si:
  //   yo puedo seguir usando el transporte para varios viajes y cada viaje tiene X paquetes. Además, puedo
  //   hacer viajes a diferentes destinos luego de regresar
  // Como está el transporte definido, solo puedo hacer UN viaje y luego tengo que perder ese viaje (pisarlo) para 
  //   poder hacer otro.
  def realizarViaje() = {
    viajeAsignado = viajeAsignado.copy(costoFacturado = costoEnvio)
    // cuidado! el acoplamiento con "Estadisticas" es fuertisimo, ya no puedo
    //  testear "Transporte" sin testearlo con LA instancia de estadisticas (no puedo hacer unit test!)
    //  sería mejor que la instancia de estadística estuviera definida por parámetro implícito
    Estadisticas.agregarViajeRealizado(viajeAsignado)
    viajeAsignado = new Viaje(Central, Mendoza, this) //Medio feo, hay que inicializarlo si o si
    // correcto, es feo asi :P y no solo eso, están definiendo UN viaje, con una dirección de origen 
    //   y destino ya definida!!!
    // como puse antes, no pueden usar definiciones estáticas en codigo de producción, estos datos
    //   dependen de la ejecución por lo que NO puede estar dentro del código productivo.
    viajeAsignado = viajeAsignado.copy(ganancia = gananciaEnvio)
  }
}