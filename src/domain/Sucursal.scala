package domain

import unidadmedida.Kilometro
import unidadmedida.VolumenM3

abstract class Sucursal(val nombre: String, var volumenDepositoSucursal: VolumenM3) {

  var transportes: Set[Transporte] = Set()
  var enviosAcumulados: Set[Envio] = Set()
  var enviosLlegandoASucursal: Set[Envio] = Set()

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

  def distanciaASucursal(otraSucursal: Sucursal): Kilometro = {
    new Kilometro(50)
  }
}

case object Central extends Sucursal(nombre = "Central", volumenDepositoSucursal = VolumenM3(1000))
case object BahiaBlanca extends Sucursal(nombre = "BahiaBlanca", volumenDepositoSucursal = VolumenM3(300))
case object Mendoza extends Sucursal(nombre = "Mendoza", volumenDepositoSucursal = VolumenM3(500))