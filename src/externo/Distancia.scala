package externo

import unidadmedida.Kilometro
import domain.BahiaBlanca
import domain.Central
import domain.Mendoza
import domain.Rio
import domain.Sucursal

// cuando se usan estas "Distancias"? para que están?
sealed abstract class Distancia(val sucursalOrigen: Sucursal, val sucursalDestino: Sucursal, val kilometros: Kilometro, val esAerea: Boolean) {

}

// No crear datos de prueba en el código producción!
//   el tp no especifica las sucursales (salvo la "casa central"), estos datos solo sirven para hacer tests
//   entonces solo deberían estar en el código de test
case object CentralCentral extends Distancia(Central, Central, Kilometro(0), false)
case object CentralBahiaBlanca extends Distancia(Central, BahiaBlanca, Kilometro(600), false)
case object CentralMendoza extends Distancia(Central, Mendoza, Kilometro(1000), false)
case object CentralRio extends Distancia(Central, Rio, Kilometro(2000), false)

case object BahiaBlancaCentral extends Distancia(CentralBahiaBlanca.sucursalOrigen, CentralBahiaBlanca.sucursalOrigen, CentralBahiaBlanca.kilometros, true)
case object BahiaBlancaBahiaBlanca extends Distancia(BahiaBlanca, BahiaBlanca, Kilometro(0), false)
case object BahiaBlancaMendoza extends Distancia(BahiaBlanca, Mendoza, Kilometro(600), false)
case object BahiaBlancaRio extends Distancia(BahiaBlanca, Rio, Kilometro(2600), false)

case object MendozaCentral extends Distancia(CentralMendoza.sucursalOrigen, CentralMendoza.sucursalOrigen, CentralMendoza.kilometros, true)
case object MendozaBahiaBlanca extends Distancia(BahiaBlancaMendoza.sucursalOrigen, BahiaBlancaMendoza.sucursalOrigen, BahiaBlancaMendoza.kilometros, true)
case object MendozaMendoza extends Distancia(Mendoza, Mendoza, Kilometro(0), false)
case object MendozaRio extends Distancia(Mendoza, Rio, Kilometro(2600), false)

case object RioCentral extends Distancia(CentralRio.sucursalOrigen, CentralRio.sucursalOrigen, CentralRio.kilometros, true)
case object RioBahiaBlanca extends Distancia(BahiaBlancaRio.sucursalOrigen, BahiaBlancaRio.sucursalOrigen, BahiaBlancaRio.kilometros, true)
case object RioMendoza extends Distancia(MendozaRio.sucursalOrigen, MendozaRio.sucursalOrigen, MendozaRio.kilometros, true)
case object RioRio extends Distancia(Rio, Rio, Kilometro(0), false)

case object CentralCentralAerea extends DistanciaAerea(CentralCentral)
case object CentralBahiaBlancaAerea extends DistanciaAerea(CentralBahiaBlanca)
case object CentralMendozaAerea extends DistanciaAerea(CentralMendoza)
case object CentralRioAerea extends DistanciaAerea(CentralRio)

case object BahiaBlancaCentralAerea extends DistanciaAerea(BahiaBlancaCentral)
case object BahiaBlancaBahiaBlancaAerea extends DistanciaAerea(BahiaBlancaBahiaBlanca)
case object BahiaBlancaMendozaAerea extends DistanciaAerea(BahiaBlancaMendoza)
case object BahiaBlancaRioAerea extends DistanciaAerea(BahiaBlancaRio)

case object MendozaCentralAerea extends DistanciaAerea(MendozaCentral)
case object MendozaBahiaBlancaAerea extends DistanciaAerea(MendozaBahiaBlanca)
case object MendozaMendozaAerea extends DistanciaAerea(MendozaMendoza)
case object MendozaRioAerea extends DistanciaAerea(MendozaRio)

case object RioCentralAerea extends DistanciaAerea(RioCentral)
case object RioBahiaBlancaAerea extends DistanciaAerea(RioBahiaBlanca)
case object RioMendozaAerea extends DistanciaAerea(RioMendoza)
case object RioRioAerea extends DistanciaAerea(RioRio)

abstract class DistanciaAerea(val distancia: Distancia) extends Distancia(distancia.sucursalOrigen, distancia.sucursalDestino, distancia.kilometros * 0.9, true)

