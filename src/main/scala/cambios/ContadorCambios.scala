package cambios

/**
 * clase para representar cambios
 *
 * @param cantidad indica la cantidad total a devolver
 * @param cambio   mapa con clave: valor de moneda y
 *                 valor: numero de monedas de este tipo
 */
case class Cambio(val cantidad: Int, val cambio: Map[Int, Int]) {
   /**
    * metodo que calcula la cantidad restante por devolver
    *
    * @return
    */
   def restante = cantidad - cambio.map(entry => entry._1 * entry._2).sum

   /**
    * se agrega una unidad mas a la moneda cuyo valor
    * se pasa como argumento. Se devuelve un nuevo objeto
    *
    * @param moneda
    */
   def agregarMoneda(moneda: Int): Cambio = {
      if (cambio.contains(moneda)) {
         new Cambio(cantidad, cambio + (moneda -> (cambio(moneda) + 1)))
      }
      else {
         new Cambio(cantidad, cambio + (moneda -> 1))
      }
   }

   /**
    * metodo toString
    *
    * @return
    */
   override def toString: String = {
      "Cambio(cantidad: " + cantidad + " restante: " + restante +
         " cambio: " + cambio.mkString(" | ")
   }


   def listarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int =
      if (cantidad == 0) 1
      else if (cantidad < 0) 0
      else if (monedas.isEmpty) 0
      else {
         val puedeUsar = (cantidad / monedas.head) > 0
         if (puedeUsar) listarCambiosPosibles(cantidad - monedas.head, monedas) + listarCambiosPosibles(cantidad, monedas.tail)
         else listarCambiosPosibles(cantidad, monedas.tail)
      }
}

/**
 * objeto para implementar los metodos de contador de cambios
 */
object ContadorCambios extends App {
   var contadorCambios = 0
   /**
    * version inicial del contador
    *
    * @param cantidad
    * @param monedas
    * @return
    */

   def listarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int =
     if (cantidad == 0) 1
     else if (cantidad < 0) 0
     else if (monedas.isEmpty) 0
     else {
       val puedeUsar = (cantidad / monedas.head) > 0
       if (puedeUsar) listarCambiosPosibles(cantidad - monedas.head, monedas) + listarCambiosPosibles(cantidad, monedas.tail)
       else listarCambiosPosibles(cantidad, monedas.tail)
     }

   var cantidad = 20
   var monedas:List[Int] = List(1, 3, 5)
   var contador = 0
   println("Hay " + listarCambiosPosibles(cantidad, monedas) + " formas distintas de devolver " + cantidad)

   val registro:Map[Int, Int] = Map()
   val cambio = Cambio(cantidad, registro)
   cambio.listarCambiosPosibles(cantidad, monedas)
   println(cambio.toString)
}
