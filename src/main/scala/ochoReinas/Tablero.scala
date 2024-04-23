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
   def agregarReina(fila : Int, columna : Int) =
      val celda = new Celda(fila, columna)
      val nuevoTablero = celda :: contenido
      nuevoTablero

   /**
    * metodo to string
    * @return
    */
   override def toString : String =
      var output = ""
      for (i <- 0 to (dimension - 1)){
         for (j <- 0 to (dimension - 1)) {
            if (contenido.contains(new Celda(i, j))) output += "\u265B"
            else if ((i % 2 == 0 && j % 2 == 0) || (i % 2 != 0 && j % 2 != 0)) output += "\u25A1"
            else output += "\u25A0"
         }
         output += "\n"
      }

      output

}

object ejecutable extends App {

   val tableroValores = List(new Celda(0, 0))
   val tablero = new Tablero(4, tableroValores)
   print(tablero.toString)
}
