package domain

import unidadmedida.Kilometro

class CalculadorDistancia {
	def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
	  Kilometro(100)
	}
	def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
	  Kilometro(1001)
	}
	def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
	  2
	}
}