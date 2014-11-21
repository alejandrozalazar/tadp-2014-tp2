package unidadmedida

case class Kilometro(val value: Double = 0) {

  def >=(vol: Kilometro): Boolean = {
    this.value >= vol.value
  }

  def *(veces: Double): Kilometro = {
    Kilometro(this.value * veces)
  }
}