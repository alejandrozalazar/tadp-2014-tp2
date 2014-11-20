import domain._
import domain.transporte._
import exceptions._
import unidadmedida._

object pruebas {

 implicit def intToUnidadesFactory(i: Double): UnidadesFactory =
    new UnidadesFactory(i)                        //> intToUnidadesFactory: (i: Double)unidadmedida.UnidadesFactory
  
  val camion2: Camion = new Camion()              //> camion2  : domain.transporte.Camion = domain.transporte.Camion@cf71b7
  camion2.agregarEnvio(new Envio(Mendoza, Central, 1.m3, Normal))
  camion2.costoEnvio                              //> res0: unidadmedida.Dinero = Dinero(10034.0)
}