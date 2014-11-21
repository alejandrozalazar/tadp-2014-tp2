package unidadmedida

class VelocidadKMH(val value: Double = 0) {

  def horasParaRecorrer(kilometros: Kilometro): Hora = {
    Hora(kilometros.value / value)
  }
}