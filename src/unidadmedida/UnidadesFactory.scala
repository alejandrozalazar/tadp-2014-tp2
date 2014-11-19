package unidadmedida

class UnidadesFactory(valor: Double) {
  def m3 = VolumenM3(valor)
  def pesos = Dinero(valor)
  def kilometros = Kilometro(valor)
  def km = Kilometro(valor)
  def kmh = VelocidadKMH(valor)
}