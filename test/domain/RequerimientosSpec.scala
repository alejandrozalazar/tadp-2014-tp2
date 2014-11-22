package domain

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import unidadmedida.UnidadesFactory
import unidadmedida.VolumenM3
import domain.Transporte
import domain.Viaje
import java.util.Date

class RequerimientosSpec extends FlatSpec with Matchers {

	implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
	implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
	
	def inicializarSucursales() = {
	  Central.viajesEsperandoPartir = List()
	  Central.viajesLlegando  = List()
	  Central.enviosRecibidos  = List()
	  Central.transportes = List()
	  
	  Rio.viajesEsperandoPartir = List()
	  Rio.viajesLlegando  = List()
	  Rio.enviosRecibidos  = List()
	  Rio.transportes = List()
	  
	  Mendoza.viajesEsperandoPartir = List()
	  Mendoza.viajesLlegando  = List()
	  Mendoza.enviosRecibidos  = List()
	  Mendoza.transportes = List()
	  
	  BahiaBlanca.viajesEsperandoPartir = List()
	  BahiaBlanca.viajesLlegando  = List()
	  BahiaBlanca.enviosRecibidos  = List()
	  BahiaBlanca.transportes = List()
	  
	}

	"1) Dado un transporte" should "saber su costo y su ganancia" in {
	  
		inicializarSucursales
		
	    Central.espacioDisponibleEnSucursal should be(1000.m3)
	    
		var camion = Camion(List(Normal), false, false)
		Central.agregarTransporte(camion)
		
		var viaje = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio1 = Envio(Rio, 10.m3, Normal, Otro)
		val envio2 = Envio(Rio, 10.m3, Normal, Otro)
		viaje = viaje.agregarEnvio(envio1)
		viaje = viaje.agregarEnvio(envio2)
    
		viaje.costoEnvio should be(10044.pesos)
		viaje.gananciaEnvio should be((-9884).pesos)
		
		Central.viajesEsperandoPartir should be(List(viaje))
		Central.espacioDisponibleEnSucursal should be(980.m3)
		Central.transportes should be(List(camion))
	}
	
	"2a) Un transporte sale hacia la sucursal" should "" in {
	
		inicializarSucursales
		
		var camion = Camion(List(Normal), false, false)
		var avion = Avion(List(Normal),false,false)
		Central.agregarTransporte(camion)
		Central.agregarTransporte(avion)
		
		var viaje1 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio1 = Envio(Rio, 10.m3, Normal, Otro)
		val envio2 = Envio(Rio, 20.m3, Normal, Otro)
		viaje1 = viaje1.agregarEnvio(envio1)
		
		viaje1 = viaje1.agregarEnvio(envio2)
		
		var viaje2 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio3 = Envio(Rio, 20.m3, Normal, Otro)
		viaje2 = viaje2.agregarEnvio(envio3)
		
		Central.viajesEsperandoPartir should be(List(viaje1,viaje2))
		
		viaje2.enviar
		
		Central.viajesEsperandoPartir should be(List(viaje1))
		Rio.viajesLlegando should be(List(viaje2))
		Central.transportes should be(List(avion))
	}
	
	"2b) Un transporte llega a la sucursal de destino" should "" in {
	
		inicializarSucursales
		
		var camion = Camion(List(Normal), false, false)
		var avion = Avion(List(Normal),false,false)
		Central.agregarTransporte(camion)
		Central.agregarTransporte(avion)
		
		var viaje1 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio1 = Envio(Rio, 10.m3, Normal, Otro)
		val envio2 = Envio(Rio, 20.m3, Normal, Otro)
		viaje1 = viaje1.agregarEnvio(envio1)
		viaje1 = viaje1.agregarEnvio(envio2)
		
		var viaje2 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio3 = Envio(Rio, 20.m3, Normal, Otro)
		viaje2 = viaje2.agregarEnvio(envio3)
		
		viaje1.enviar
		viaje1.llegarADestino
		
		Rio.enviosRecibidos  should be(List(envio2,envio1))
		
	}

	"2c) Un transporte llega nuevamente a la sucursale de destino" should "" in {
	
		inicializarSucursales
		
		var camion = Camion(List(Normal), false, false)
		var avion = Avion(List(Normal),false,false)
		Central.agregarTransporte(camion)
		Central.agregarTransporte(avion)
		
		var viaje1 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio1 = Envio(Rio, 10.m3, Normal, Otro)
		val envio2 = Envio(Rio, 20.m3, Normal, Otro)
		viaje1 = viaje1.agregarEnvio(envio1)
		viaje1 = viaje1.agregarEnvio(envio2)
		
		var viaje2 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio3 = Envio(Rio, 20.m3, Normal, Otro)
		viaje2 = viaje2.agregarEnvio(envio3)
		
		viaje1.enviar
		viaje1.llegarADestino
		
		Central.transportes should be(List(avion))
		
		viaje1.regresarASucursal
		
		Central.transportes should be(List(avion,camion))
		
	}
	
	"3) Se retira un envio de la sucursal" should "" in {
	
		inicializarSucursales
		
		var camion = Camion(List(Normal), false, false)
		var avion = Avion(List(Normal),false,false)
		Central.agregarTransporte(camion)
		Central.agregarTransporte(avion)
		
		var viaje1 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio1 = Envio(Rio, 10.m3, Normal, Otro)
		val envio2 = Envio(Rio, 20.m3, Normal, Otro)
		viaje1 = viaje1.agregarEnvio(envio1)
		viaje1 = viaje1.agregarEnvio(envio2)
		
		var viaje2 = Viaje(camion, Central, Nil, new Date(2014,1,1))
		val envio3 = Envio(Rio, 20.m3, Normal, Otro)
		viaje2 = viaje2.agregarEnvio(envio3)
		
		viaje1.enviar
		viaje1.llegarADestino
		viaje1.regresarASucursal
		
		Rio.enviosRecibidos  should be(List(envio2,envio1))
		
		Rio.retirarEnvio(envio2)
		
		Rio.enviosRecibidos  should be(List(envio1))
	}
	
	//Para el requerimiento de las estadisticas, revisar el test EstadisitcasSpec
		
		
}