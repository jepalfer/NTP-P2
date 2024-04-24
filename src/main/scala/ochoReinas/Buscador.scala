package ochoReinas

import scala.annotation.tailrec

class Buscador (val dimension : Int) {
  var contadorTableros = 0
   /**
    * metodo para ubicar nueva reina
    * @param fila
    * @return
    */

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
      go(filaInicial, columnaInicial, contadorInicial, Tablero(dimension, List()))
      
}
