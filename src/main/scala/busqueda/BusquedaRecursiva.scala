package busqueda

object BusquedaRecursiva extends App {
   var contadorSaltos = 0
   var contadorBinaria = 0
   var contadorFibonacci = 0
   def esMayor[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.gt(x, y)

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
                         (esMayor: (A, A) => Boolean): Int = ???


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
   println (resultadoSaltos + " encontrado en " + contadorSaltos + " llamadas recursivas");
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
      var inicio = -1

      def go(indice: Int, inicio: Int, f0: Int, f1: Int): Int =
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

      go (0, 0, 0, 0)

}   
