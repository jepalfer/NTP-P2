package ochoReinas

import scala.annotation.tailrec

/**
 * clase para representar celdas del tablero
 *
 * @param fila
 * @param columna
 */
case class Celda(val fila : Int, val columna : Int)

/**
 * objeto con metodos para determinar conflicto entre celdas
 */
object Conflicto {

   /**
    * determina si se genera conflicto al agregar una nueva reina
    * en la celda indicada
    * @param celda
    * @param tablero
    * @return
    */
   def conflictoCeldaTablero(celda : Celda, tablero : Tablero) : Boolean =

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca horizontalmente
       * (fila) con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaFila(fila: Int, columna: Int) : Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (columna < tablero.dimension) compruebaFila(fila, columna + 1)
            else false
         }

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca verticalmente
       * (columna) con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaColumna(fila: Int, columna: Int) : Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (fila < tablero.dimension) compruebaColumna(fila + 1, columna)
            else false
         }

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca en la diagonal
       * de arriba izquierda con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaDiagonalAI(fila: Int, columna: Int) : Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (fila >= 0 && columna >= 0) compruebaDiagonalAI(fila - 1, columna - 1)
            else false
         }

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca en la diagonal
       * de arriba derecha con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaDiagonalaI(fila: Int, columna: Int): Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (fila < tablero.dimension && columna >= 0) compruebaDiagonalaI(fila + 1, columna - 1)
            else false
         }

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca en la diagonal
       * de abajo izquierda con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaDiagonalAD(fila: Int, columna: Int) : Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (fila >= 0 && columna < tablero.dimension) compruebaDiagonalAD(fila - 1, columna + 1)
            else false
         }

      /**
       * metodo auxiliar para comprobar que la reina que estamos colocando no se choca en la diagonal
       * de abajo derecha con otra reina
       * @param fila fila de la celda que estamos comprobando
       * @param columna columna de la celda que estamos comprobando
       * @return
       */
      def compruebaDiagonalaD(fila: Int, columna: Int) : Boolean =
         if (tablero.contenido.contains(new Celda(fila, columna))) true
         else {
            if (fila < tablero.dimension && columna < tablero.dimension) compruebaDiagonalaD(fila + 1, columna + 1)
            else false
         }

      //1. Comprobamos la fila de la celda en la que queremos colocar la reina
      val hayReinaEnFila = compruebaFila(celda.fila, 0)

      //2. Comprobamos la columna de la celda en la que queremos colocar la reina
      val hayReinaEnColumna = compruebaColumna(0, celda.columna)

      //3. Comprobamos las diagonales de la celda en la que queremos colocar la reina
      val hayReinaEnDiagonal1 = compruebaDiagonalAI(celda.fila - 1, celda.columna - 1)
      val hayReinaEnDiagonal2 = compruebaDiagonalAD(celda.fila - 1, celda.columna + 1)
      val hayReinaEnDiagonal3 = compruebaDiagonalaI(celda.fila + 1, celda.columna - 1)
      val hayReinaEnDiagonal4 = compruebaDiagonalaD(celda.fila + 1, celda.columna + 1)

      //4. Hacemos la comprobación de las colisiones para determinar si se puede colocar o no una reina
      if (!hayReinaEnFila && !hayReinaEnColumna && !hayReinaEnDiagonal1 && !hayReinaEnDiagonal2 &&
        !hayReinaEnDiagonal3 && !hayReinaEnDiagonal4) false
      else true
}

/**
 * clase para representar el tablero
 * @param dimension numero de filas y columnas
 * @param contenido contenido del tablero, solo de las
 *                  celdas ocupadas
 */
class Tablero(val dimension : Int, val contenido : List[Celda]) {

   /**
    * se agrega nueva reina al tablero y se genera un tablero
    * nuevo
    * @param fila
    * @param columna
    * @return
    */
   def agregarReina(fila : Int, columna : Int) = new Tablero(dimension, new Celda(fila, columna) ::contenido)

   /**
    * metodo to string
    * @return
    */
   override def toString : String =
      var output = ""
      for (i <- (dimension - 1) to (0) by -1){
         for (j <- (0) to (dimension - 1)) {
            if (contenido.contains(new Celda(i, j))) output += "\u265B\t"
            else if ((i % 2 == 0 && j % 2 == 0) || (i % 2 != 0 && j % 2 != 0)) output += "\u25A0\t"
            else output += "\u25A1\t"
         }
         output += "\n"
      }

      output

}

object Reinas extends App {
   val dimension = 6
   val buscador = new Buscador(dimension)
   val tablero = buscador.resolver
   println("------- tablero de tamaño " + dimension + "x" + dimension + " resuelto -------")
   print(tablero.toString)
   println("----------------------------------------------")
}
