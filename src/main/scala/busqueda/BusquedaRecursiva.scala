package busqueda

object BusquedaRecursiva extends App {
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
                        (esMayor: (A, A) => Boolean): Int = ???


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
                           (esMayor: (A, A) => Boolean): Int = ???
}   
