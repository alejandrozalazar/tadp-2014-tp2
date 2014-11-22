package domain

import unidadmedida.Kilometro
import unidadmedida.VolumenM3
import unidadmedida.UnidadesFactory
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

abstract class Sucursal(val nombre: String, var volumenDepositoSucursal: VolumenM3, val pais: String) {

  
  implicit def intToUnidadesFactory(i: Int): UnidadesFactory =
    new UnidadesFactory(i)
    
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)
  
  var viajesLlegando: List[Viaje] = List()
  var viajesEsperandoPartir: List[Viaje] = List()
  var enviosRecibidos: List[Envio] = List()
  
  var transportes: List[Transporte] = List()
 
  
  def espacioDisponibleEnSucursal: VolumenM3 = {
    
   val enviosTotales  = ((viajesLlegando ++ viajesEsperandoPartir).flatMap(viaje => viaje.envios )) ++ enviosRecibidos
   println("Envios totales size = " ++ enviosTotales.size.toString)
   val espacioOcupado = enviosTotales.foldLeft(0.m3){(volumen,envio) => volumen + envio.volumen}
   this.volumenDepositoSucursal - espacioOcupado
  }
  
  def retirarEnvio(envio:Envio) = {
    enviosRecibidos = enviosRecibidos.filter(envioLista => envioLista != envio)
  }
  
  def agregarTransporte(transporte:Transporte) = {
    transportes = transportes ++ List(transporte)
  }

  def quitarTransporte(transporte:Transporte) = {
    transportes  = transportes.filter(transporteLista => !transporteLista.equals(transporte))
  }
}

case object Central extends Sucursal(nombre = "Central", volumenDepositoSucursal = VolumenM3(1000), pais = "Argentina")
case object BahiaBlanca extends Sucursal(nombre = "BahiaBlanca", volumenDepositoSucursal = VolumenM3(300), pais = "Argentina")
case object Mendoza extends Sucursal(nombre = "Mendoza", volumenDepositoSucursal = VolumenM3(500), pais = "Argentina")
case object Rio extends Sucursal(nombre = "Rio de Janeiro", volumenDepositoSucursal = VolumenM3(800), pais = "Brasil")