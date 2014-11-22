package domain

import unidadmedida.Kilometro
import unidadmedida.VolumenM3
import scala.collection.mutable.HashSet

abstract class Sucursal(val nombre: String, var volumenDepositoSucursal: VolumenM3, val pais: String) {

  //var transportes: Set[Transporte] = Set()
  var enviosAcumulados: HashSet[Viaje] = HashSet()
  var enviosLlegandoASucursal: HashSet[Viaje] = HashSet()
  var enviosRecibidos: HashSet[Viaje] = HashSet()
  
  def espacioDisponibleEnSucursal: VolumenM3 = {
    val enviosAcumuladosEnSucursal = enviosAcumulados

    val espacioDisponibleMenosEnviosAcumulados = (enviosAcumuladosEnSucursal.foldLeft(volumenDepositoSucursal) { (volumenRestante, envio) =>
      volumenRestante - envio.volumen
    })

    val espacioDisponibleEnSucursal = (enviosLlegandoASucursal.foldLeft(espacioDisponibleMenosEnviosAcumulados) { (volumenRestante, envio) =>
      volumenRestante - envio.volumen
    })
    espacioDisponibleEnSucursal
  }

}

case object Central extends Sucursal(nombre = "Central", volumenDepositoSucursal = VolumenM3(1000), pais = "Argentina")
case object BahiaBlanca extends Sucursal(nombre = "BahiaBlanca", volumenDepositoSucursal = VolumenM3(300), pais = "Argentina")
case object Mendoza extends Sucursal(nombre = "Mendoza", volumenDepositoSucursal = VolumenM3(500), pais = "Argentina")
case object Rio extends Sucursal(nombre = "Rio de Janeiro", volumenDepositoSucursal = VolumenM3(800), pais = "Brasil")