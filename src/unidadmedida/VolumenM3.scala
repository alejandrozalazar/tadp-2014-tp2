package unidadmedida

// lo mismo que en "Dinero", reimplementarlo no tiene mucha utilidad
case class VolumenM3(val value: Double = 0) {

  def <=(vol: VolumenM3): Boolean = {
    this.value <= vol.value
  }

  def -(vol: VolumenM3): VolumenM3 = {
    VolumenM3(this.value - vol.value)
  }

  def +(vol: VolumenM3): VolumenM3 = {
    VolumenM3(this.value + vol.value)
  }

  def >(vol: VolumenM3): Boolean = {
    this.value > vol.value
  }
}