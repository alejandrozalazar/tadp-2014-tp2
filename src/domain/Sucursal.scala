package domain

import unidadmedida.VolumenM3
import unidadmedida.VolumenM3
import unidadmedida.Kilometro

class Sucursal {

  var volumenDepositoSucursal: VolumenM3 = VolumenM3(0)
  var transportes: Set[Transporte] = Set()
  var enviosAcumulados: Set[Envio] = Set()
  var enviosLlegandoASucursal: Set[Envio] = Set()

  def espacioDisponibleEnSucursal:VolumenM3 = {
    val enviosAcumuladosEnSucursal = enviosAcumulados

    val espacioDisponibleMenosEnviosAcumulados = (enviosAcumuladosEnSucursal.foldLeft(volumenDepositoSucursal) { (volumenRestante, envio) =>
      volumenRestante - envio.volumen
    })

    val espacioDisponibleEnSucursal = (enviosLlegandoASucursal.foldLeft(espacioDisponibleMenosEnviosAcumulados) { (volumenRestante, envio) =>
      volumenRestante - envio.volumen
    })
    espacioDisponibleEnSucursal
  }
  
  def distanciaASucursal (otraSucursal: Sucursal):Kilometro = {
    new Kilometro(50)
  }
}