package unidadmedida

class UnidadesFactory(valor: Double) {
  def m3 = VolumenM3(valor)
  def pesos = Dinero(valor)
}