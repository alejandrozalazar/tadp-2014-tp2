package unidadmedida

import unidadmedida.Hora

case class VelocidadKMH(val value: Double = 0) {

  def horasParaRecorrer(kilometros: Kilometro)= { 
    Hora(kilometros.value / value)
   } 

}