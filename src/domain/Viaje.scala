package domain

import java.util.Date
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.TransporteNoPoseeInfraestructura
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.Kilometro
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import unidadmedida.Hora
import unidadmedida.UnidadesFactory
import java.util.Calendar
import scala.math.BigDecimal

case class Viaje(val transporte: Transporte, val sucursalOrigen: Sucursal, val envios: List[Envio], val fechaSalida: Date) {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  def puedeLlevarEnviosUrgentes: Boolean = false
  def puedeLlevarEnviosFragiles: Boolean = false
  def poseeRefrigeracion: Boolean = false

  def volumen:VolumenM3 = {
    envios.foldLeft(0.m3) { (volumenTotal, envio) => volumenTotal + envio.volumen }
  }
  
  // ENVIAR -----------------------------------------
  def enviar = {
    Estadisticas.agregarViajeRealizado(this)  		
  }
  
  //-------------------------------------------------
  
  def precioEnvio(): Dinero = {
    val sumatoriaPrecio = envios.foldLeft(0.pesos) { (precioTotal, envio) => precioTotal + envio.precio }
    sumatoriaPrecio
  }
  
  // CALCULO DEL TIEMPO -------------------------------------------------------------------------------------
  def duracion():Hora = {
    val distancia = distanciaEntre(sucursalOrigen, destino) * 2 // ida y vuelta 
    val tiempo = transporte.velocidad.horasParaRecorrer(distancia) 
    redondear(tiempo.value).hora
    

  }
  
  def redondear(value:Double):Double = {
    BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_DOWN ).toDouble
  }
  
  // --------------------------------------------------------------------------------------------------------
  
  // CALCULO DE GANANCIA ------------------------------------------------------------------------------------------------------
  def gananciaEnvio(): Dinero = {
    val sumatoriaPrecios = envios.foldLeft(0.pesos) { (costoTotal, envio) => costoTotal + envio.precio }
    sumatoriaPrecios - costoEnvio
  }
  //-----------------------------------------------------------------------------------------------------------------

  //AGREGAR UN ENVIO ---------------------------------------------------------------------------------------------------------
  def agregarEnvio(envio: Envio): Viaje = {
    validarEnvio(envio)
    var viaje:Viaje = null
    envios match {
      case Nil => viaje = Viaje(transporte, sucursalOrigen, List(envio), fechaSalida)
      case xs => {
        validarMismaSucursalEnvios(envio)
        viaje = Viaje(transporte, sucursalOrigen: Sucursal, List(envio) ++ envios, fechaSalida)
        sucursalOrigen.enviosAcumulados.remove(this)
      }
    }
    sucursalOrigen.enviosAcumulados.add(viaje)
    viaje
  }
  
  
  // ---------------------------------------------------------------------------------------------------------

  //CALCULO DE COSTOS -----------------------------------------------------------------------------------------------------
  def costoEnvio(): Dinero = {
    var costo = costoDistancia
    costo = costo + costoPaquetes
    if (transporte.tieneGps) { costo = costo + ((distanciaEntre(sucursalOrigen, destino)).value * 2 * 0.5).pesos }
    if (transporte.tieneSeguimientoSatelital) { costo = costo + ((distanciaEntre(sucursalOrigen, destino)).value * 2 * 3.74).pesos }
    if (tieneSustanciasPeligrosas) { costo = costo + 600.pesos }
    if (tieneAnimales) {
      if (distanciaEntre(sucursalOrigen, destino) < 100.km) { costo = costo + 50.pesos }
      if (distanciaEntre(sucursalOrigen, destino) >= 100.km && distanciaEntre(sucursalOrigen, destino) <= 200.km) { costo = costo + 86.pesos }
      if (distanciaEntre(sucursalOrigen, destino) > 200.km) { costo = costo + 137.pesos }
    }

    transporte match {

      case Avion(enviosSoportados, tieneGps, tieneSeguimientoSatelital) => {
        if (envioAOtroPais) { costo = costo * 1.10 } //Costo por distinto pais
        if (volumenRestante.value >= transporte.capacidad.value * 0.8) { costo = costo * 3 }
        if(pasadoElDia20 && destino.equals(Central)){costo = costo*1.2}
      }

      case Camion(enviosSoportados, tieneGps, tieneSeguimientoSatelital) => {
        costo = costo + (cantidadDePeajes * 12).pesos //CostoPeajes
        costo = costo + costoPorEnviosConRefrigeraicion
        if ((volumenRestante.value >= transporte.capacidad.value * 0.8) && !destino.equals(Central) && !sucursalOrigen.eq(Central)) {
          costo = costo * (1 + (transporte.capacidad - volumenRestante).value / transporte.capacidad.value)
        }
        if (tieneSustanciasPeligrosas && tienePaquetesUrgentes) { costo = costo + 3.pesos * (volumenPaquetesUrgentes.value / transporte.capacidad.value) }
        if (esUltimaSemana && destino.equals(Central)) {costo = costo*1.02}
      }
      
      case Furgoneta(enviosSoportados, tieneGps, tieneSeguimientoSatelital) => {
        costo = costo + (cantidadDePeajes * 6).pesos //CostoPeajes
        if ((volumenRestante.value >= transporte.capacidad.value * 0.8) && tieneAlMenosTresPaquetesUrgentes) {
          costo = costo * 2
        }

      }
       
    }
    redondear(costo.value).pesos
  }
  def costoDistancia(): Dinero = {
    val distancia = distanciaEntre(sucursalOrigen, destino)
    Dinero(transporte.costoPorKilometro.value * distancia.value) // TODO
  }
  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    transporte match {
      case Avion(_, _, _) => {
        new CalculadorDistancia().distanciaAereaEntre(origen, destino)
      }
      case _ => {
        new CalculadorDistancia().distanciaTerrestreEntre(origen, destino)
      }
    }
  }
  def destino = this.envios.head.sucursalDestino

  def costoPaquetes(): Dinero = {
    envios.map(a => a.costo).foldLeft(0.pesos)((costo1, costo2) => costo1 + costo2)
  }
  def costoPeajes(): Dinero = {
    transporte match {
      case Avion(_, _, _) => 0.pesos
      case Camion(_, _, _) => Dinero(new CalculadorDistancia().cantidadPeajesEntre(sucursalOrigen, destino) * 12)
      case Furgoneta(_, _, _) => Dinero(new CalculadorDistancia().cantidadPeajesEntre(sucursalOrigen, destino) * 6)
    }
  }

  def tieneSustanciasPeligrosas(): Boolean = {
    envios.map(envio => envio.naturaleza).contains(SustanciaPeligrosa)
  }
  def tieneAnimales(): Boolean = {
    envios.map(envio => envio.naturaleza).contains(Animal)
  }
  def tienePaquetesUrgentes(): Boolean = {
    envios.map(envio => envio.tipoEnvio).contains(Urgente)
  }
  def volumenPaquetesUrgentes: VolumenM3 = {
    envios.filter(envio => envio.tipoEnvio.equals(Urgente)).map(envio => envio.volumen).foldLeft(0.m3)((v1, v2) => v1 + v2)
  }

  def envioAOtroPais: Boolean = {
    !sucursalOrigen.pais.equals(envios.head.sucursalDestino.pais)
  }

  def cantidadDePeajes: Int = {
    new CalculadorDistancia().cantidadPeajesEntre(sucursalOrigen, destino)
  }
  def costoPorEnviosConRefrigeraicion: Dinero = {
    envios.filter(envio => envio.tipoEnvio.equals(NecesitaRefrigeracion)).map(a => 5.pesos).foldLeft(0.pesos)((costo1, costo2) => costo1 + costo2)
  }

  def tieneAlMenosTresPaquetesUrgentes: Boolean = {
    envios.count(envio => envio.tipoEnvio.equals(Urgente)) < 3
  }

  def esUltimaSemana():Boolean = {
    var calendar = Calendar.getInstance();
    calendar.setTime(fechaSalida);
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    calendar.add(Calendar.MONTH, 1);
    calendar.set(Calendar.DAY_OF_MONTH, 1);
    calendar.add(Calendar.DATE, -1);
    var ultimoDia = calendar.get(Calendar.DAY_OF_MONTH);
    var diferencia = ultimoDia - miDia;
    diferencia <= 7;
  }
  
  def pasadoElDia20():Boolean = {
     var calendar = Calendar.getInstance();
     calendar.setTime(fechaSalida);
     //calendar.set(fechaSalida.get(Calendar.YEAR), fechaSalida.get(Calendar.MONTH), fechaSalida.get(Calendar.DAY_OF_MONTH)); 
     var dia = calendar.get(Calendar.DAY_OF_MONTH)

     this.destino.equals(Central) && dia >= 20 

  }

  //-----------------------------------------------------------------------------------------------------------------------

  //VALIDACIONES ----------------------------------------------------------------------------------------------------------
  def validarEnvio(envio: Envio) = {
    validar(puedeTransportarVolumen(envio.volumen), TransporteTieneVolumenInsuficienteParaRealizarElEnvio())
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio), TransporteNoSoportaElTipoEnvioEspecificado())
    validar(sucursalDestinoTieneSuficienteEspacio(envio.volumen, envio.sucursalDestino), LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible())
  }

  def validarMismaSucursalEnvios(envio: Envio) = {
    validar((this.envios.head.sucursalDestino.nombre.equals(envio.sucursalDestino.nombre)), TransporteNoSeDirigeALaSucursalDeDestinoEspecificada())
  }

  def puedeTransportarVolumen(volumen: VolumenM3): Boolean =
    {
      volumen <= (volumenRestante)
    }
  
  def sucursalDestinoTieneSuficienteEspacio(volumen: VolumenM3, sucursalDestino: Sucursal): Boolean = 
    { 
       volumen <= sucursalDestino.espacioDisponibleEnSucursal 
    } 


  def volumenRestante: VolumenM3 = {
    transporte.capacidad - envios.map(a => a.volumen).foldLeft(0.m3)((v1, v2) => v1 + v2)
  }

  def validar(resultadoValidacion: Boolean, exception: ValidacionException): Boolean = {
    if (!resultadoValidacion) {
      throw exception
    }
    resultadoValidacion
  }

  def puedeManejarElTipoDeEnvio(tipoEnvioAValidar: TipoEnvio): Boolean = {
    transporte.enviosSoportados.contains(tipoEnvioAValidar)
  }
  
  
  
}