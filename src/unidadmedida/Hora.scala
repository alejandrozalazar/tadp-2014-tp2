package unidadmedida

case class Hora(val value: Double = 0) { 

 
   def +(hora: Hora): Hora = { 
     Hora(this.value + hora.value) 
   } 
 
 
   def -(hora: Hora): Hora = { 
     Hora(this.value - hora.value) 
   } 
 
 
   def *(veces: Double): Hora = { 
     Hora(this.value * veces) 
   } 
 
 
   def /(veces: Double): Hora = { 
     Hora(this.value / veces) 
   } 
 
 
   def round() = { 
     Hora(this.value.round) 
   } 
}