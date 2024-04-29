package busqueda

object BusquedaRecursiva extends App {
   def esMayor[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.gt(x, y)
   def esMenor[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.lt(x, y)
   def esMayorIgual[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.gteq(x, y)

   /**
    * Metodo binario de busqueda
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   @annotation.tailrec
   def busquedaBinaria[A](coleccion: List[A], aBuscar: A)
                         (esMayor: (A, A) => Boolean)
                         (inicio: Int, fin: Int): Int =

      val medio = inicio + (fin - inicio) / 2
      if (inicio > fin) -1
      else if (coleccion(medio).equals(aBuscar)) medio
      else if (esMayor(coleccion(medio), aBuscar)) busquedaBinaria(coleccion, aBuscar)(esMayor)(inicio, medio - 1)
      else busquedaBinaria(coleccion, aBuscar)(esMayor)(medio + 1, fin)

   var Lista2:List[Int] = List(0, 1, 2, 3, 4, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
   var buscado = 3
   var resultadoBinaria = busquedaBinaria[Int](Lista2, buscado)(esMayor)(0, Lista2.length - 1)
   println ("Valor => " + buscado + " encontrado en el índice => " + resultadoBinaria)

   /**
    * Metodo de busqueda a saltos
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   def busquedaSaltos[A](coleccion: List[A], aBuscar: A)
                        (esMayor: (A, A) => Boolean): Int =

      def busquedaLineal(indice: Int, fin: Int): Int =
         if (indice >= coleccion.length || indice > fin) -1
         else {
            if (coleccion(indice) == aBuscar) indice
            else busquedaLineal(indice + 1, fin)
         }

      def go(inicio: Int, fin: Int, tamBloque: Int): Int =
         val indice = Math.min(fin, coleccion.length - 1)
         //println("Fin => " + fin + "\nColeccion(indice) => " + coleccion(indice) + "\ntamBloque => " + tamBloque)
         if (esMayor(aBuscar, coleccion(indice))){
            if (!(aBuscar.equals(coleccion(indice))) && indice == (coleccion.length - 1)) - 1
            else {
               //println("Indice => " + indice)
               //println("Entro")
               go(fin + 1, fin + tamBloque - 1, tamBloque)
            }
         }
         else busquedaLineal(inicio, fin)

      val tamBloque = Math.sqrt(coleccion.length).toInt
      val inicio = 0
      val fin = tamBloque - 1
      go(inicio, fin, tamBloque)

   val lista = List.range(0, 3000)
   println(Math.sqrt(lista.length).toInt)
   println("---------------------")
   println("SALTOS")
   println("---------------------")
   for (i <- 0 until(lista.length)){
      //println("Valor => " + lista(i) + " encontrado en el índice => " + busquedaSaltos[Int](lista, lista(i))(esMayor))
   }

   var Lista1:List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13)
   buscado = 2
   var resultadoSaltos = busquedaSaltos[Int](Lista1, buscado)(esMayor)
   println ("Valor => " + buscado + " encontrado en el índice => " + resultadoSaltos)
   /**
    * metodo de busqueda guiada por la serie de Fibonacci
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   def busquedaFibonacci[A](coleccion: List[A], aBuscar: A)
                           (esMayor: (A, A) => Boolean)(implicit numeric: Numeric[A]): Int =
      import numeric._
      def go(indice: Int, inicio: Int, f0: Int, f1: Int): Int =
         val nuevoIndice = Math.min(inicio + f0, coleccion.length - 1)

         if (f0 <= 0) {
            if (aBuscar.equals(coleccion.last)) go(coleccion.length - 1, inicio, f0, f1)//nuevoIndice = coleccion.length - 1
            else -1

            //go(nuevoIndice, inicio, f0, f1)
         }
         else {
            if (coleccion(nuevoIndice).equals(aBuscar)) nuevoIndice
            else {
               if (esMayor(aBuscar, coleccion(nuevoIndice))) go(nuevoIndice, nuevoIndice, f1 - f0, f0)
               else go(nuevoIndice, inicio, f0 - (f1 - f0), f1 - f0)
            }
         }

      def busquedaLineal(indice: Int, secuencia: List[A]): Int =
         if (esMayor(aBuscar, secuencia(indice))) indice
         else busquedaLineal(indice + 1, secuencia)

      //Funcion hace fibonacci


      def fibonacci[A](n: A, prev: A, act: A)(implicit numeric: Numeric[A]): A =
         import numeric._
         if (n == zero) prev
         else fibonacci(n - one, act, act + prev)

      var listaInicial:List[A] = List()

      def generaListaFibonacci(listaFibonacci: List[A], valor: A)(implicit numeric: Numeric[A]): List[A] =
         //import numeric._
         val nuevoFibonacci = fibonacci(valor, zero, one)
         val nuevaLista = listaFibonacci :+ nuevoFibonacci
         if (esMayorIgual(nuevoFibonacci.toInt, coleccion.length)) nuevaLista
         else generaListaFibonacci(nuevaLista, valor + one)
      val listaFinal = generaListaFibonacci(listaInicial, numeric.zero)

      //for (i <- 0 to listaFinal.length - 1)
      //   println(listaFinal(i))
      //println("Ultimo elemento fib => " + listaFinal.last)

      //Llamada a la funcion
      //Obtenemos el mayor inmediatamente a aBuscar

      val inicio = -1
      val f1 = listaFinal(listaFinal.length - 2)
      val f0 = listaFinal(listaFinal.length - 3)
      //println("f1 => " + f1.toString + "\nf0 => " + f0.toString)
      go(inicio, inicio, f0.toInt, f1.toInt)

   println("---------------------")
   println("FIBONACCI")
   println("---------------------")

   for (i <- 0 until (lista.length)) {
     // println("Valor => " + lista(i) + " encontrado en el índice => " + busquedaFibonacci[Int](lista, lista(i))(esMayor))
   }
   var Lista3:List[Int] = List(0, 1, 2, 3, 4, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
   buscado = 8
   var resultadoFibonacci = busquedaFibonacci[Int](Lista3, buscado)(esMayor)
   println ("Valor => "+ buscado + " está en el índice => " + resultadoFibonacci)

}   
