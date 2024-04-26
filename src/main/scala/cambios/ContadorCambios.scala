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
object ContadorCambios extends App {
   var contadorCambios = 0
   /**
    * version inicial del contador
    *
    * @param cantidad
    * @param monedas
    * @return
    */
   /*
   def listarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {
      if (monedas.nonEmpty) println("Entro en listar cambios con moneda " + monedas.head)
      if (cantidad == 0 || monedas.isEmpty) 0
      else {
         val monedasNuevas = monedas
         val monedaActual = monedas.head
         val cuantasCambia = (cantidad / monedas.head).toInt

         def go(cantidad: Int, monedasActual: List[Int]): Int = {
            if (cantidad == 0) {
               println("Primer elemento de monedas " + monedasActual(0))
               1
            }
            else if (cantidad < 0) 0
            else {
               println("Entro aqui con moneda " + monedasActual.head)
               val monedaActual = monedasActual.head
               val cuantasCambia = (cantidad / monedasActual.head).toInt
               if (cuantasCambia == 0 && monedasActual.tail.nonEmpty) {
                  println("entro en el if importante con moneda " + monedasActual.head + " y monedas(1) = " + monedasActual(1))
                 println(monedas.head)
                 go(cantidad, monedas.filterNot(_ == monedasActual.head))
               } else {
                 go(cantidad - monedasActual.head, monedasActual)
               }
            }
         }

         contadorCambios += go(cantidad - monedas.head, monedas)
         listarCambiosPosibles(cantidad, monedas.tail)
      }
   }


   listarCambiosPosibles(10, List(1, 2, 3, 4))
*/

   def listarCambiosPosibles(cantidad: Int, monedas: List[Int], contador: Int): Int =
     def go(cantidadActual: Int, contadorActual: Int): Int =
       val nuevasMonedas = monedas.filter(moneda => moneda <= cantidadActual)
       println(nuevasMonedas(contadorActual) + " - " + cantidadActual)

       print("Cantidad => " + cantidadActual + " || monedas actual => ")
       for (i <- 0 until nuevasMonedas.length)
         print(nuevasMonedas(i) + " ")

       println()
       val nuevaCantidad = cantidadActual - nuevasMonedas(contadorActual)
       if (nuevaCantidad == 0) {
         contadorCambios += 1
         val proximaCantidad = cantidadActual + nuevasMonedas(contadorActual)
         println("hemos llegado a 0")
         go(proximaCantidad, contadorActual + 1)
       }
       else {
         if (nuevaCantidad > 0 && nuevasMonedas.isEmpty) go(cantidadActual, contadorActual + 1)
         else go(nuevaCantidad, 0)
       }

     val nuevasMonedas = monedas.filter(moneda => moneda <= cantidad)
     contadorCambios += go(cantidad, contador)
     if (contador < nuevasMonedas.length - 1) {
       listarCambiosPosibles(cantidad, nuevasMonedas, contador + 1)
     }
     else contadorCambios

   var cantidad = 20
   var monedas:List[Int] = List(2, 5, 20)
   var contador = 0
   println(listarCambiosPosibles(cantidad, monedas, 0))
}
