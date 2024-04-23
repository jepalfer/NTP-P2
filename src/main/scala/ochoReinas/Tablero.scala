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
   def conflictoCeldaTablero(celda : Celda, tablero : Tablero) : Boolean = ???
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
   def agregarReina(fila : Int, columna : Int) = ???

   /**
    * metodo to string
    * @return
    */
   override def toString : String = ???
}
