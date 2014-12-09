package unidadmedida

// No termino de entender cual es la ganancia de wrapear el double...
//   ahora solo pueden usar las operaciones definidas en este tipo
// (podrían haber usado un type alias si querían dar información de tipo pero sin reimplementarlo)
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

  def /(veces: Double): Dinero = {
    Dinero(this.value / veces)
  }

  def round() = {
    Dinero(this.value.round)
  }
}