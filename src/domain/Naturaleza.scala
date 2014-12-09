package domain

class Naturaleza {}

case object Animal extends Naturaleza
case object SustanciaPeligrosa extends Naturaleza
// que parte del TP define "Otro"? si quieren un "no" valor miren el tipo Option
case object Otro extends Naturaleza
