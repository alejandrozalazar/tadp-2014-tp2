package unidadmedida

case class Dinero(val value: Double = 0) {

  def +(vol: Dinero): Dinero = {
    Dinero(this.value + vol.value)
  }

  def -(vol: Dinero): Dinero = {
    Dinero(this.value - vol.value)
  }
}