package domain

import scala.reflect.runtime.universe._
import unidadmedida.Kilometro
import unidadmedida.UnidadesFactory

trait CalculadorDistancia {
  implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)

  // NO! esto es un trait que alguien (que no somos nosotros) implementa y nos lo tiene que pasar
  //  para poder ejecutar. Estos valores solo sirven para hacer test, por lo que NO pueden estar
  //  en el código productivo!
  // El codigo productivo solo hace referencia al trait pero no va a definir ninguna
  //  implementación, quien lo use tendrá que instancias algo que lo implemente.
  def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
    100.kilometros
  }
  def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Kilometro = {
    1001.kilometros
  }
  def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
    2
  }
}

