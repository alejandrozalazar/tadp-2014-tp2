package domain

import unidadmedida.Kilometro
import unidadmedida.VolumenM3

abstract class Sucursal(val nombre: String, var volumenDepositoSucursal: VolumenM3, val pais: String) {

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
  
}

// La única sucursal definida en el TP es la "casa central", el resto solo es útil para testear, no pueden estar en el código productivo
//  (además, revisen si pueden separar a la definición de cual es la casa central de su instancia / sus valores, como el volumen, pais, etc)
case object Central extends Sucursal(nombre = "Central", volumenDepositoSucursal = VolumenM3(1000), pais = "Argentina")
case object BahiaBlanca extends Sucursal(nombre = "BahiaBlanca", volumenDepositoSucursal = VolumenM3(300), pais = "Argentina")
case object Mendoza extends Sucursal(nombre = "Mendoza", volumenDepositoSucursal = VolumenM3(500), pais = "Argentina")
case object Rio extends Sucursal(nombre = "Rio de Janeiro", volumenDepositoSucursal = VolumenM3(800), pais = "Brasil")