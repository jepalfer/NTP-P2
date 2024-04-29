package ochoReinas

import scala.annotation.tailrec

class Buscador (val dimension : Int) {
  var contadorTableros = 0
   /**
    * metodo para ubicar nueva reina
    * @param fila
    * @return
    */

  /*
def resolver : Tablero =
   def go (fila: Int, columna: Int, contador: Int = 0, tablero: Tablero): Tablero =
      if (contador == tablero.dimension) tablero
      else {
        if (!Conflicto.conflictoCeldaTablero(new Celda(fila, columna), tablero) &&
             fila < tablero.dimension && columna < tablero.dimension)
          println("Nueva reina colocada en " + fila + " - " + columna)
          go(fila + 1, 0, contador + 1, tablero.agregarReina(fila, columna))
        else {
           if (columna == tablero.dimension - 1)
             contadorTableros += 1
             go(0, contadorTableros, 0, new Tablero(dimension, List()))
           else go(fila, columna + 1, contador, tablero)
        }
      }

   val filaInicial = 0
   val columnaInicial = 0
   val contadorInicial = 0
   val columnasIniciales = List.fill(dimension)(0)
   go(filaInicial, columnaInicial, contadorInicial, Tablero(dimension, List()))
*/

   def resolver: Tablero =
     /**
      * Método auxiliar para la resolución del tablero
      *
      * @param fila              fila de la celda en la que queremos colocar la reina
      * @param columna           columna de la celda en la que queremos colocar la reina
      * @param contador          contador de reinas colocadas
      * @param tablero           tablero con el que estamos trabajando en el momento
      * @param columnasColocadas lista auxliar para hacer backtracking al tablero anterior
      * @return tablero con una solución válida al problema de las N reinas
      */
     @annotation.tailrec
     def go(fila: Int, columna: Int, contador: Int = 0, tablero: Tablero, columnasColocadas: List[Int]): Tablero =
        //Si hemos colocado todas las reinas devolvemos el tablero
       if (contador == tablero.dimension) tablero
       else {
         if (!Conflicto.conflictoCeldaTablero(new Celda(fila, columna), tablero) &&
           fila < tablero.dimension && columna < tablero.dimension) //Si no nos hemos salido de los límites y no hay colisión de reina

           //Guardamos la columna en la que hemos colocado esta reina
           val nuevasColumnas = columnasColocadas.updated(fila, columna)

           //Usamos recursión para pasar a la siguiente fila
           go(fila + 1, 0, contador + 1, tablero.agregarReina(fila, columna), nuevasColumnas)
         else {
           //Si no hemos podido colocar una reina en la fila
           if (columna == tablero.dimension) {
             println("Tablero actual. . .")
             println(tablero.toString)

             //Hacemos backtracking
             go(fila - 1, columnasColocadas(fila - 1) + 1, contador - 1, new Tablero(dimension, tablero.contenido.tail), columnasColocadas)
           } else go(fila, columna + 1, contador, tablero, columnasColocadas) //Si no hemos podido colocar una reina pero queda fila por delante
         }
       }

     //Inicializamos variables
     val filaInicial = 0
     val columnaInicial = 0
     val contadorInicial = 0
     val columnasIniciales = List.fill(dimension)(0)
     
     //Hacemos la llamada al método auxiliar
     go(filaInicial, columnaInicial, contadorInicial, Tablero(dimension, List()), columnasIniciales)

}
