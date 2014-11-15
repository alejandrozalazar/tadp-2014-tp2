package unidadmedida

case class Dinero(val value: Double = 0) {

  def +(dinero: Dinero): Dinero = {
    Dinero(this.value + dinero.value)
  }

  def -(dinero: Dinero): Dinero = {
    Dinero(this.value - dinero.value)
  }
  
  def *(veces: Double): Dinero = {
    Dinero(this.value * veces)
  }
}