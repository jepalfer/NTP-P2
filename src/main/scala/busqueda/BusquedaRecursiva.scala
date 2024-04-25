package busqueda

object BusquedaRecursiva extends App {
   var contadorSaltos = 0
   var contadorBinaria = 0
   var contadorFibonacci = 0
   def esMayor[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.gt(x, y)
   def esIgual[A](x: A, y: A) = x.equals(y)
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
   def busquedaBinaria[A](coleccion: List[A], aBuscar: A)
                         (esMayor: (A, A) => Boolean, esIgual: (A, A) => Boolean): Int =

      @annotation.tailrec
      def busquedaBinariaRecursiva(inicio: Int, fin: Int): Int =
         val medio = inicio + (fin - inicio) / 2
         if (inicio > fin) -1
         else if (esIgual(coleccion(medio), aBuscar)) medio
         else if (esMayor(coleccion(medio), aBuscar)) busquedaBinariaRecursiva(inicio, medio - 1)
         else busquedaBinariaRecursiva(medio + 1, fin)

      busquedaBinariaRecursiva(0, coleccion.length - 1)

   var Lista2:List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
   val buscado = 13
   var resultadoBinaria = busquedaBinaria[Int](Lista2, buscado)(esMayor, esIgual)
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
      val tamBloque = Math.sqrt(coleccion.length).toInt
      val inicio = 0
      val fin = tamBloque - 1

      def busquedaLineal(indice: Int, fin: Int): Int =
         println(indice)
         if (indice >= coleccion.length || indice > fin) -1
         else {
            if (coleccion(indice) == aBuscar) indice
            else busquedaLineal(indice + 1, fin)
         }

      def go(inicio: Int, fin: Int): Int =
         contadorSaltos += 1
         val indice = Math.min(fin, coleccion.length - 1)
         if (esMayor(aBuscar, coleccion(indice)) && fin <= coleccion.length) go(fin + 1, fin + tamBloque - 1)
         else busquedaLineal(inicio, fin)

      go(inicio, fin)

   var Lista1:List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
   var resultadoSaltos = busquedaSaltos[Int](Lista1, 1)(esMayor)
   println (resultadoSaltos + " encontrado en " + contadorSaltos + " llamadas recursivas")
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
                           (esMayor: (A, A) => Boolean): Int =
      val inicio = -1

      def go(indice: Int, inicio: Int, f0: Int, f1: Int): Int =
         contadorFibonacci += 1
         var nuevoIndice = Math.min(inicio + f0, coleccion.length - 1)
         if (f0 == 0) {
            if (aBuscar == coleccion.last) nuevoIndice = coleccion.length - 1
            else nuevoIndice = -1

            go(nuevoIndice, inicio, f0, f1)
         }
         else {
            if (coleccion(nuevoIndice) == aBuscar) nuevoIndice
            else {
               if (esMayor(aBuscar, coleccion(nuevoIndice))) go(nuevoIndice, indice, f1 - f0, f1 - f0)
               else go(nuevoIndice, inicio, f0 - (f1 - f0), f1 - (f0 - (f1 - f0)))
            }
         }

      def busquedaLineal(indice: Int, secuencia: List[A]): Int =
         if (esMayor(aBuscar, secuencia(indice))) indice
         else busquedaLineal(indice + 1, secuencia)
      //Funcion hace fibonacci
      def fibonacci(n: Int, prev: Int, act: Int): Int =
         if (n == 0) prev
         else fibonacci(n - 1, act, act + prev)

      var listaInicial:List[Int] = List()
      val inicial = 0
/*
      def generaListaFibonacci(listaFibonacci: List[Int], valor: Int): List[Int] =
         val nuevoFibonacci = fibonacci(valor, 0, 1)
         val nuevaLista = listaFibonacci :+ nuevoFibonacci
         if (esMayorIgual(nuevoFibonacci, aBuscar)) nuevaLista
         else generaListaFibonacci(nuevaLista, valor + 1)

      listaInicial = generaListaFibonacci(listaInicial, inicial)
      
*/
      //Llamada a la funcion
      //Obtenemos el mayor inmediatamente a aBuscar

      val f0 = 5
      val f1 = 8
      go (inicio, inicio, f0, f1)

   var Lista3:List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   //var Lista3:List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
   var resultadoFibonacci = busquedaFibonacci[Int](Lista3, 3)(esMayor)
   println (resultadoFibonacci + " encontrado en " + contadorFibonacci + " llamadas recursivas")
}   
