package exceptions

abstract class ValidacionException extends Exception

// Detalle: Es raro tener exceptions sin datos que indiquen porqué se generaron, generalmente incluyen 
//   a una excepción anterior (la cause de ésta) o tienen datos de esta isntancia (ej. un mensaje, el valor
//   de lo que falló, etc).

// detalle de scala: las case classes sin parámetros no tienen sentido (para que quiero multiples instancias
//  de algo que siempre va a ser igual (no está parametrizado). Entonces puedo usar directamente case object
case object TransporteTieneVolumenInsuficienteParaRealizarElEnvio extends ValidacionException
case class TransporteNoSoportaElTipoEnvioEspecificado() extends ValidacionException
case class TransporteNoSeDirigeALaSucursalDeDestinoEspecificada() extends ValidacionException
case class LaSucursalDeDestinoNoTieneSuficienteEspacioDisponible() extends ValidacionException
case class TransporteNoPoseeInfraestructura() extends ValidacionException