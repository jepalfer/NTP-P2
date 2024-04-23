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
}

/**
 * objeto para implementar los metodos de contador de cambios
 */
object ContadorCambios {
   /**
    * version inicial del contador
    *
    * @param cantidad
    * @param monedas
    * @return
    */
   def listarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = ???
}
