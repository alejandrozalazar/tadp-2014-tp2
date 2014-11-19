package domain

import java.util.Date
import exceptions.LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible
import exceptions.TransporteNoPoseeInfraestructura
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.TransporteNoSoportaElTipoEnvioEspecificado
import exceptions.TransporteTieneVolumenInsuficienteParaRealizarElEnvio
import exceptions.TransporteNoSeDirigeALaSucursalDeDestinoEspecificada
import exceptions.ValidacionException
import unidadmedida.CostoPorKM
import unidadmedida.Dinero
import unidadmedida.Kilometro
import unidadmedida.VelocidadKMH
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory
import domain.transporte.Avion
import domain.transporte.Camion
import domain.transporte.Furgoneta
import java.util.Calendar


abstract class Transporte(val sucursal:Sucursal, val envios:List[Envio], val tiposDeEnviosSoportados:List[TipoEnvio], val tieneGps:Boolean, val tieneSeguimientoSatelital: Boolean, val fechaSalida: Date) {

  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  val capacidad = 0.m3
  val velocidad = 0.kmh
  val costoPorKilometro = 0.pesos
  
  def enviosSoportados: List[TipoEnvio] = tiposDeEnviosSoportados
  
  //val envios: List[Envio]
  
  def puedeLlevarEnviosUrgentes: Boolean = false
  def puedeLlevarEnviosFragiles: Boolean = false
  def poseeRefrigeracion: Boolean = false  
  
  //CALCULO DE COSTOS -----------------------------------------------------------------------------------------------------
  def costoEnvio(): Dinero = {
    var costo = costoDistancia
    costo = costo + costoPaquetes
    if (tieneGps){costo = costo *(distanciaEntre(sucursal, destino)).value * 2 * 0.5}
    if (tieneSeguimientoSatelital){costo = costo *(distanciaEntre(sucursal, destino)).value * 2 * 3.74}
    if (tieneSustanciasPeligrosas) {costo + 600.pesos}
    if (tieneAnimales){
      if (distanciaEntre(sucursal, destino) < 100.km) {costo + 50.pesos}
      if (distanciaEntre(sucursal, destino) >= 100.km && distanciaEntre(sucursal, destino) <= 200.km) {costo + 86.pesos}
      if (distanciaEntre(sucursal, destino) > 200.km) {costo + 137.pesos}      
    }
    
    this match {
      case Avion(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => {
        if (envioAOtroPais) {costo = costo*1.10} //Costo por distinto pais
        if (volumenRestante.value >= capacidad.value*0.8) {costo = costo*3}
        costo
      }
      
      case Camion(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => {
        costo = costo + (cantidadDePeajes * 12).pesos //CostoPeajes
        costo = costo + costoPorEnviosConRefrigeraicion
        if ((volumenRestante.value >= capacidad.value*0.8) && !destino.equals(Central) && !sucursal.eq(Central)){
          costo = costo * (1 + (capacidad - volumenRestante).value/capacidad.value)
        }
        if (tieneSustanciasPeligrosas && tienePaquetesUrgentes){ costo = costo + 3.pesos * (volumenPaquetesUrgentes.value / capacidad.value)}
        costo
      }
      case Furgoneta(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => {
        costo = costo + (cantidadDePeajes * 6).pesos //CostoPeajes
        costo = costo + costoPorEnviosConRefrigeraicion
        if ((volumenRestante.value >= capacidad.value*0.8) && tieneAlMenosTresPaquetesUrgentes) {
          costo = costo * 2
        }
        costo
        
      }
    }
    //costoDistancia + costoPaquetes + costoPeajes + costosExtra(costoPaquetes) + costoVolumen(costoPaquetes) + costoServiciosExtra + costoInfraestructura
  }
  def costoDistancia(): Dinero = {
    val distancia = distanciaEntre(sucursal, destino)
    Dinero(costoPorKilometro.value * distancia.value) // TODO
  }
  def distanciaEntre(origen: Sucursal, destino: Sucursal): Kilometro = {
    this match {
      case Avion(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => {
        new CalculadorDistancia().distanciaAereaEntre(origen, destino)       
      }
      case _ => {
        new CalculadorDistancia().distanciaTerrestreEntre(origen, destino)
      }
    }
  }
  def destino = this.envios.head.sucursalDestino
  
  def costoPaquetes(): Dinero = {
    envios.map(a => a.costo).foldLeft(0.pesos)((costo1,costo2)=> costo1 + costo2)
  }
  def costoPeajes(): Dinero = {
    this match {
      case Avion(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => 0.pesos
      case Camion(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => Dinero(new CalculadorDistancia().cantidadPeajesEntre(sucursal, destino) * 12)
      case Furgoneta(sucursal,envios,enviosSoportados,tieneGps,tieneSeguimientoSatelital,fechaSalida) => Dinero(new CalculadorDistancia().cantidadPeajesEntre(sucursal, destino) * 6)
    }
  }
  
  def tieneSustanciasPeligrosas():Boolean = {
   envios.map(envio => envio.naturaleza).contains(SustanciaPeligrosa)
  }
  def tieneAnimales():Boolean = {
   envios.map(envio => envio.naturaleza).contains(Animal)
  }
  def tienePaquetesUrgentes():Boolean = {
   envios.map(envio => envio.tipoEnvio).contains(Urgente)
  }
  def volumenPaquetesUrgentes:VolumenM3 = {
    envios.filter(envio => envio.tipoEnvio.equals(Urgente)).map(envio => envio.volumen).foldLeft(0.m3)((v1,v2) => v1 + v2)
    }
  
  def envioAOtroPais: Boolean = {
    !sucursal.pais.equals(envios.head.sucursalDestino.pais)
  }
  
  def cantidadDePeajes: Int = {
    new CalculadorDistancia().cantidadPeajesEntre(sucursal, destino)
  }
  def costoPorEnviosConRefrigeraicion: Dinero = {
    envios.filter(envio => envio.tipoEnvio.equals(NecesitaRefrigeracion)).map(a => 5.pesos).foldLeft(0.pesos)((costo1,costo2)=> costo1 + costo2)
  }
  
  def tieneAlMenosTresPaquetesUrgentes: Boolean = {
    envios.count(envio => envio.tipoEnvio.equals(Urgente)) < 3
  }
  
    def costoFinDeMes(costo:Dinero) = {
    var calendar = Calendar.getInstance();  
    calendar.setTime(fechaSalida);
    var miDia = calendar.get(Calendar.DAY_OF_MONTH);
    calendar.add(Calendar.MONTH, 1);  
    calendar.set(Calendar.DAY_OF_MONTH, 1);  
    calendar.add(Calendar.DATE, -1);
    var ultimoDia = calendar.get(Calendar.DAY_OF_MONTH);
    
    var diferencia = ultimoDia - miDia;
    var estaEnLaUltimaSemana = (diferencia <= 7);
    
    if(this.destino.equals(Central) && estaEnLaUltimaSemana){
      costo * 0.02
    } else 0.pesos
  }
  
  //-----------------------------------------------------------------------------------------------------------------------
    
  //VALIDACIONES ----------------------------------------------------------------------------------------------------------
  def validarEnvio(envio:Envio) = {
    validar(puedeTransportarVolumen(envio.volumen), TransporteTieneVolumenInsuficienteParaRealizarElEnvio())
    validar(puedeManejarElTipoDeEnvio(envio.tipoEnvio), TransporteNoSoportaElTipoEnvioEspecificado())
  }
  
  def validarMismaSucursalEnvios(envio:Envio) = {
    validar((this.envios.head.sucursalDestino.nombre.equals(envio.sucursalDestino.nombre)),TransporteNoSeDirigeALaSucursalDeDestinoEspecificada())
  }
  
  def puedeTransportarVolumen(volumen: VolumenM3): Boolean =
    {
      volumen <= (volumenRestante)
    }
  
  def volumenRestante: VolumenM3 = {
   capacidad - envios.map(a => a.volumen).foldLeft(0.m3)((v1,v2)=> v1 + v2)
  }
  
  def validar(resultadoValidacion: Boolean, exception: ValidacionException): Boolean = {
    if (!resultadoValidacion) {
      throw exception
    }
    resultadoValidacion
  }
  
  def puedeManejarElTipoDeEnvio(tipoEnvioAValidar: TipoEnvio): Boolean = {
    this.enviosSoportados.contains(tipoEnvioAValidar)
  }
  //---------------------------------------------------------------------------------------------
  
  /*
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

  

 

  def volumenOcupado: VolumenM3 = {
    capacidad - volumenRestante
  }

  def puedeLlevarEnviosUrgentes: Boolean = false
  def puedeLlevarEnviosFragiles: Boolean = false
  def poseeRefrigeracion: Boolean = false



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

  
  
  def costoPaquetes() = viajeAsignado.costoPaquetes

  

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
    viajeAsignado = new Viaje(Central, Mendoza, this)

  }
  */
}