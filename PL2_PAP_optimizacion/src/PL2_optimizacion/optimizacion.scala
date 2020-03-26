package PL2_optimizacion

object optimizacion {
  
  
  //OPTIMIZACION

  val colores: List[Char] = List('A','N','R','V','M','G') //Lista de colores posibles
  
  val tablero = List(
             List('O','O','O','O','O','O','O','O','O'), //posiciones del 0 al 8
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O'),
             List('O','O','O','O','O','O','O','O','O')
             ) 

       
    //Marina y Nicol
  def main(args: Array[String]) 
    {
    //MECANICA
    //Al comenzar la partida se llena el tablero con 9 bolas
    val tableroLleno = llenar_tablero(0,9,tablero)
    println("Tablero inicial: ")
    imprimir_tablero(tableroLleno)
    //Se inicia el juego
    //jugar(tableroLleno, 0) 
    println(estrategia(tableroLleno, colores,0,0,0,' '))
    //println("salsdhoaoidpfia")
     
         
    }
  
  //Marina
  def elegirBola(tablero: List[List[Char]]): List[Int] = //pide por consola la fila y la columna de la bola que se desea mover
  {
    try
    {
      println("Introduzca la FILA (0-8) de la bola que desea MOVER")
     val fila = scala.io.StdIn.readInt()
     println("Introduzca la COLUMNA (0-8) de la bola que desea MOVER")
     val columna = scala.io.StdIn.readInt()
     println("\n")
      
     if(fila < 9 && columna < 9 && columna >= 0 && fila >= 0) //si la fila esta en el rango válido (0-8)
     {
       if (tablero(fila)(columna)!= 'O') //Si hay una bola en esa posición
       {
         val pos = List(fila, columna)
         pos
       }
       else //Si no hay bola se pide otra
       {
         println("MOVIMIENTO INCORRECTO, INTRODUZCA OTRO")
         elegirBola(tablero)
       }
     }
     else //Si el valor introducido esta fuera de rango
     {
       println("FILA O COLUMNA FUERA DE RANGO")
       elegirBola(tablero) //Se pide otro valor 
     }

    }catch 
    {
      case e: NumberFormatException =>  { //Si el valor introducido no es un numero
        println("FORMATO NO VALIDO")
        elegirBola(tablero)
      }
    }
  }
  
  //Marina
  def elegirPosicion(tablero: List[List[Char]]): List[Int] = //pide por consola la fila y la columna en la que se desea colocar la bola
  {
    try
    {
       println("Introduzca la FILA (0-8) en la que desea COLOCAR la bola")
       val fila = scala.io.StdIn.readInt()//posicion leida
       println("Introduzca la COLUMNA (0-8) en la que desea COLOCAR la bola")
       val columna= scala.io.StdIn.readInt()
       println("\n")
       if(fila < 9 && columna < 9 && columna >= 0 && fila >= 0)
       {
         if (tablero(fila)(columna)== 'O') //Si no hay una bola en esa posición
         {
           val pos = List(fila, columna)
           pos
         }
         else //Si la posicion esta ocupada
         {
           println("MOVIMIENTO INCORRECTO, INTRODUZCA OTRO")
           elegirPosicion(tablero)
         }
       }
       else //Si los numeros introduccidos no son validos
       {
         println("FILA O COLUMNA FUERA DE RANGO")
         elegirPosicion(tablero)
       }
     }catch
     {
       case e: NumberFormatException =>  { //Si el valor introducido no es un numero
       println("FORMATO NO VALIDO")
       elegirPosicion(tablero)
       }
     }
     
  }
  
  //Nicol
  def jugar(tablero: List[List[Char]], puntos: Int): List[List[Char]] =
  {
    if(esMiembro(tablero, 'O')) //Si quedan huecos libres
    { //Continua la partida
     val pos = elegirBola(tablero)
     val fila = pos(0)
     val columna = pos(1)
     val posNueva = elegirPosicion(tablero)
     val filaNueva = posNueva(0)
     val columnaNueva = posNueva(1)
     //Se mueve la bola a la posición indicada
     val tablero2 = mover_bola(fila, columna, filaNueva ,columnaNueva, tablero)
     val color = tablero2(filaNueva)(columnaNueva)
     val tablero3 = actualizar_tablero(filaNueva, columnaNueva, color , tablero2) //comprobar si se ha hecho alguna fila columna o diagonal de 5 y eliminar bolas
     imprimir_tablero(tablero3) 
     val puntos2 = get_puntos(filaNueva, columnaNueva, color , tablero2) + puntos //se actualiza el marcador con los puntos de esta ronda
     println("PUNTOS: "+ puntos2) 
     jugar(tablero3, puntos2) //se inicia otra ronda
    
    }
    else //Si no quedan huecos en el tablero se acaba la partida
    {
      println("Fin de la partida")
      println(tablero)
      tablero
    }
  }
  
  //Nicol
  def imprimir_tablero(tablero: List[List[Char]]): Any = { //imprime el tablero con formato 
    
    if(!tablero.isEmpty)
    {
    val fila = tablero.head
    imprimir_fila(fila)
    imprimir_tablero(tablero.tail)
    }
    else
    {
      print("\n")
    }

  }
  //Nicol
  def imprimir_fila(fila: List[Char]): Any = { //metodo auxiliar que imprime una fila
    if(!fila.isEmpty)
    {
       val elem = fila.head
       print(elem + " ")
       val tail = fila.tail
       imprimir_fila(tail)
    }
    else
    {
      print("\n")
    }
   
    
  }
  
  //Nicol
  def esMiembro( lista: List[List[Char]], elem: Char): Boolean = { //comprueba si elem es miembro del tablero
    if(!lista.isEmpty)
    {
      val fila = lista.head
      if(esMiembro_fila(fila, elem)) //comprueba el primer elemento
      {
         true
      }
      else
      {
        esMiembro(lista.tail, elem) //comprueba la cola
      }
      
    }
    else
    {
      false
    }
  }
  
  //Nicol
 def esMiembro_fila( lista: List[Char], elem: Char): Boolean = { //comprueba si elem es miembro de una fila
    if(!lista.isEmpty)
    {
      val item = lista.head
      if(item == elem) //si el el primer elemento coincide con elem
      {
         true
      }
      else
      {
        esMiembro_fila(lista.tail, elem) //comprueba la cola
      }
      
    }else
    {
      false
    }
 }
  
    //marina
     def insertar_aux (color: Char, pos: Int, tablero: List[Char]): List[Char] = { //inserta un char en una lista
          if (pos==0) color::tablero.tail
          else tablero.head::insertar_aux(color, pos-1, tablero.tail)
          } 
     
    //marina 
    def insertar_aux2 (lista: List[Char], pos: Int, tablero: List[List[Char]]): List[List[Char]] = { //inserta una lista en el tablero
          if (pos==0) lista::tablero.tail
          else tablero.head::insertar_aux2(lista, pos-1, tablero.tail)
          } 
  
    //marina
   def insertar (color: Char, fila: Int, columna: Int, tablero: List[List[Char]]): List[List[Char]] = { 
     //actualiza el tablero dadas la fila y columna en las que se quiere insertar una bola y el color de esta
     val filaNueva:List[Char] = insertar_aux(color, columna, tablero(fila))
     insertar_aux2(filaNueva, fila, tablero) 

      }

      //nicol y marina
   def llenar_tablero(inicio: Int, numBolas: Int, tablero: List[List[Char]]): List[List[Char]] = { 
     if(esMiembro(tablero, 'O'))
     {
       if (inicio < numBolas) //si todavia no se han metido el numero de bolas deseado
       {
         val color_aleatorio = scala.util.Random.nextInt(5) //se genera una bola de color aleatorio
         val columna = scala.util.Random.nextInt(9) //y posicion aleatoria
         val fila = scala.util.Random.nextInt(9) 
         val bola = colores(color_aleatorio)
         if(tablero(fila)(columna) == 'O') //si la posicion aleatoria generada esta vacia
         {
           val aux = insertar(bola,fila,columna,tablero) //se introduce la bola en el tablero
           llenar_tablero(inicio+1, numBolas, aux) //llamada recursiva para seguir introduciendo bolas
         }
         else
         {
           llenar_tablero(inicio, numBolas, tablero)
         }

       }
       else
       {
         tablero //retorna el tablero 
       }
     }
     else
     {
       println("NO QUEDAN HUECOS")
       tablero
     }
       
 
   }
   
   //marina
   def mover_bola(filaIni: Int, columnaIni: Int, filaFin: Int ,columnaFin: Int, tablero: List[List[Char]]): List[List[Char]] = { //mueve bola de (filaIni, columnaIni) a (filaFin, columnaFin)
     val bola = tablero(filaIni)(columnaIni) //obtener bola que se quiere mover
     val aux = insertar(bola, filaFin, columnaFin, tablero) //insertar bola adecuada en posicion final
     insertar('O', filaIni, columnaIni, aux)   //vaciar posicion inicial
     
   }
   //NICOL y marina
   def elementos_fila_izq(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //saca una lista con las posiciones  consecutivas del mismo color, comprobando posiciones desde el color hacia la izquierda
   {
     if(columna > 0) //siempre y cuando la columna sea mayor que 0
     {
       if(tablero(fila)(columna-1) == color)
       {
        val pos = List(fila,columna-1)
        val lista_nueva = pos :: lista
        elementos_fila_izq(fila, columna-1,color, tablero,lista_nueva)
       
       }
       else
       {
         lista
       }
     }
     else
     {
     lista
     }
    
   }
   //NICOL y marina
    def elementos_fila_drch(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //saca una lista con las posiciones  consecutivas del mismo color, comprobando posiciones desde el color hacia la derecha

     if(columna < 8)
     {
       if(tablero(fila)(columna +1) == color)
       {
        val pos = List(fila,columna+1)
        val lista_nueva = pos :: lista
        elementos_fila_drch(fila, columna+1,color, tablero,lista_nueva)
       
       }
       else //si no es del mismo color para de comprobar ya que no serian consecutivas y devuelve la lista
       {
         lista
       }
     }
     else 
     {
       lista
     }
    }

    //NICOL y marina 
    def get_bolas_fila(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve una lista con las fichas consecutivas del mismo color de una misma fila
    {
       val elementos_drch = elementos_fila_drch(fila,columna,color,tablero,Nil)
       val elementos_izq = elementos_fila_izq(fila,columna,color,tablero,Nil) 
       val posActual = List(fila, columna)
       val lista2 = posActual :: elementos_izq
       val listaConsecutivas = lista2 ++ elementos_drch
       listaConsecutivas

     }
    
    //NICOL y marina
   def elementos_columna_arriba(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //saca una lista con las posiciones  consecutivas del mismo color, comprobando posiciones desde la posicion seleccionada hacia arriba
   {
     if(fila > 0) //si la fila no es la fila superior
     {
       if(tablero(fila-1)(columna) == color) //comprueba si la bola de arriba es del mismo color
       {
        val pos = List(fila-1,columna)
        val lista_nueva = pos :: lista
        elementos_columna_arriba(fila-1, columna,color, tablero,lista_nueva)
       
       }
       else //si no es del mismo color para de comprobar ya que no serian consecutivas y devuelve la lista
       {
         lista
       }
     } //si no hay mas filas arriba para de comprobar y devuleve la lista
     else
     {
     lista
     }
    
   }
   //NICOL y marina
    def elementos_columna_abajo(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //saca una lista con las posiciones  consecutivas del mismo color, comprobando posiciones desde la posicion seleccionada hacia abajo
       if(fila < 8) //si la fila no es la fila inferior del tablero
     {
       if(tablero(fila+1)(columna) == color)
       {
        val pos = List(fila+1,columna)
        val lista_nueva = pos :: lista
        elementos_columna_abajo(fila+1, columna,color, tablero,lista_nueva)
       }
       else //si no es del mismo color para de comprobar ya que no serian consecutivas y devuelve la lista
       {
         lista
       }
       
     }
     else
     {
       lista
     }
    }
    
    //NICOL y marina
    def get_bolas_columna(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve una lista con las fichas consecutivas del mismo color de una misma columna
    {
       val elementos_abajo = elementos_columna_abajo(fila,columna,color,tablero,Nil)
       val elementos_arriba = elementos_columna_arriba(fila,columna,color,tablero,Nil)
       val posActual = List(fila, columna)
       val lista2 = posActual :: elementos_abajo
       val listaConsecutivas = lista2 ++ elementos_arriba
       listaConsecutivas

     }
    
    //NICOL
    def elementos_diagonal_abajo_drch2(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //2 diagonal que mira primero abajo y luego derecha
       if((fila < 8) && (columna < 8)) //se compruba que la fila es menor que 8 y la columna tambien
      {
        val fila_aux = fila + 1
        if(tablero(fila_aux)(columna) == color) //comprobamos que la bola de abajo sea del mismo color para poder formar la diagonal
        {
          val pos = List(fila_aux,columna)
          val lista_nueva = pos :: lista //en ese caso, metemos la posicion en la lista que vamos a devolevr
          
          val columna_aux = columna + 1
          
          if(tablero(fila_aux)(columna_aux) == color) //comprobamos que la bola de la derecha de la bola comprobada antes sea del mismo color para poder formar la diagonal
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva //metemos la posicion en la lista
            elementos_diagonal_abajo_drch2(fila_aux,columna_aux,color,tablero,lista_nueva2) // si se cumple lo anterior, llamamso recurivamente con la fila y la columna nueva 
          }
          else //si la bola de la derecha de la bola comprobada anterioremente no es del mismo color
          {
            lista_nueva //devolvemos la lista donde unicamente a�adimos la posicion de la primera bola comprobada en esta iteraci�n (bola de abajo)
          }
        }
        else // si la bola de abajo no es del mismo color
        {
          lista //devolvemos la lista sin a�adir nada mas
        }
      }
      else //si fila y columna no estan en el rango
      {
         lista
      }
    }
    
    //NICOL
    def elementos_diagonal_abajo_drch1(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //1 diagonal que mira primero derecha y luego abajo
       if((fila < 8) && (columna < 8))
      {
        val columna_aux = columna + 1
        if(tablero(fila)(columna_aux) == color)
        {
          val pos = List(fila,columna_aux)
          val lista_nueva = pos :: lista
          
          val fila_aux = fila + 1
          
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_abajo_drch1(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
    
    
    //NICOL
     def elementos_diagonal_arriba_drch2(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //2 diagonal que mira primero derecha y luego arriba
       if((fila > 0) && (columna < 8))
      {
       
        val columna_aux = columna + 1
        if(tablero(fila)(columna_aux) == color)
        {
          val pos = List(fila,columna_aux)
          val lista_nueva = pos :: lista
          
          val fila_aux = fila - 1
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_arriba_drch2(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
     
     //marina
      def elementos_diagonal_arriba_drch1(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    { //1 diagonal que mira primero arriba y luego derecha
       if((fila > 0) && (columna < 8))
      {
       
        val fila_aux = fila - 1
        if(tablero(fila_aux)(columna) == color)
        {
          val pos = List(fila_aux,columna)
          val lista_nueva = pos :: lista
          
          val columna_aux = columna + 1
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_arriba_drch1(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
     
     //NICOL
     def elementos_diagonal_abajo_izq2(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve una lista con las posiciones de las bolas del mismo color que forman una diagonal (SEGUN EL PROFE)
    { //2 diagonal que mira primero abajo y luego izquierda
       
      if((fila < 8) && (columna > 0))
      {
        val fila_aux = fila + 1
        if(tablero(fila_aux)(columna) == color)
        {
          val pos = List(fila_aux,columna)
          val lista_nueva = pos :: lista
          
          val columna_aux = columna - 1
          
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_abajo_izq2(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
    
     //Marina
     def elementos_diagonal_abajo_izq1(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve una lista con las posiciones de las bolas del mismo color que forman una diagonal (SEGUN EL PROFE)
    { //1 diagonal que mira primero izquierda y luego abajo
       
      if((fila < 8) && (columna > 0))
      {
        val columna_aux = columna - 1
        if(tablero(fila)(columna_aux) == color)
        {
          val pos = List(fila,columna_aux)
          val lista_nueva = pos :: lista
          
          val fila_aux = fila + 1
          
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_abajo_izq1(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
     
    //NICOL
     def elementos_diagonal_arriba_izq2(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    {//2 diagonal que mira primero izquierda y luego arriba
        if((fila > 0) && (columna > 0))
      {
       
        val columna_aux = columna - 1
        if(tablero(fila)(columna_aux) == color)
        {
          val pos = List(fila,columna_aux)
          val lista_nueva = pos :: lista
          
          val fila_aux = fila - 1
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_arriba_izq2(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
     
    //Marina
    def elementos_diagonal_arriba_izq1(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
    {//1 diagonal que mira primero arriba y luego izquierda
        if((fila > 0) && (columna > 0))
      {
       
        val fila_aux = fila - 1
        if(tablero(fila_aux)(columna) == color)
        {
          val pos = List(fila_aux,columna)
          val lista_nueva = pos :: lista
          
          val columna_aux = columna - 1
          if(tablero(fila_aux)(columna_aux) == color)
          {
            val pos = List(fila_aux,columna_aux)
            val lista_nueva2 = pos :: lista_nueva
            elementos_diagonal_arriba_izq1(fila_aux,columna_aux,color,tablero,lista_nueva2)
          }
          else
          {
            lista_nueva
          }
        }
        else
        {
          lista
        }
      }
      else
      {
         lista
      }
    }
    
    //marina
    def elegir_diagonalID(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve la diagonal de izquierda a derecha mas larga 
    {
       val diagonal_ID1 = elementos_diagonal_arriba_izq1(fila,columna,color,tablero,Nil) ++ elementos_diagonal_abajo_drch1(fila,columna,color,tablero,Nil)
       val diagonal_ID2 = elementos_diagonal_arriba_izq2(fila,columna,color,tablero,Nil) ++ elementos_diagonal_abajo_drch2(fila,columna,color,tablero,Nil)
       if(diagonal_ID1.length > diagonal_ID2.length) //Si la 1 es mas larga devolvemos la 1
       {
         diagonal_ID1
       }
       else //si la dos es mas larga devolvemos la 2
       {
         diagonal_ID2
       }
    }
    
    //marina
     def elegir_diagonalDI(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve la diagonal de derecha a izquierda mas larga
    {
       val diagonal_DI1 = elementos_diagonal_arriba_drch1(fila,columna,color,tablero,Nil) ++ elementos_diagonal_abajo_izq1(fila,columna,color,tablero,Nil)
       val diagonal_DI2 = elementos_diagonal_arriba_drch2(fila,columna,color,tablero,Nil) ++ elementos_diagonal_abajo_izq2(fila,columna,color,tablero,Nil)
       
       if(diagonal_DI1.length > diagonal_DI2.length)
       {
         diagonal_DI1
       }
       else
       {
         diagonal_DI2
       }
    }
    
    //marina
   def get_bolas_diagonal(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //en caso de que haya dos diagonales respecto a la misma bola se elige la mas larga
    {
       val diagonalDI = elegir_diagonalDI(fila, columna, color, tablero, Nil)
       val diagonalID = elegir_diagonalID(fila, columna, color, tablero, Nil)
       val posActual = List(fila, columna) 
       
       if(diagonalDI.length > diagonalID.length)
       {
         posActual::diagonalDI
       }
       else
       {
         posActual::diagonalID
       }


     } 
     
  //NICOL
  def eliminar_bolas(lista: List[List[Int]],tablero: List[List[Char]]): List[List[Char]] =
    // dada una lista con las posiciones de bolas consecutivas del mismo color, sustituye dichas posiciones en el tablero por 0
  {
    if(!lista.isEmpty)
    {
      val posicion = lista.head //cogemos el primer elemento de la lista, cuya primera posicion es la fila y la segunda la columna
      val fila = posicion(0)
      val columna = posicion(1)
      val nuevo_tablero= insertar('O',fila,columna,tablero) //insertamos una O en esa posicion
      eliminar_bolas(lista.tail,nuevo_tablero) //llamamos con el resto de la lista
    }
    else
    {
      tablero
    }
   
  }
     
  //MARINA y nicol
  def actualizar_tablero(fila: Int, columna: Int, color: Char, tablero: List[List[Char]]):  List[List[Char]] =
  {//actualiza el tablero tras realizarse un movimiento
    val consecutivasCol = get_bolas_columna(fila,columna,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
    val consecutivasFila =  get_bolas_fila(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
    val consecutivasDiagonal = get_bolas_diagonal(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
    
    if(consecutivasCol.length >= 5) //si hay mas de 5 bolas 
    { 
      eliminar_bolas(consecutivasCol,tablero) //Se eliminan

    }
    else if(consecutivasFila.length >= 5)
    { 
      eliminar_bolas(consecutivasFila,tablero)

    }
    else if(consecutivasDiagonal.length >= 5)
    {
      eliminar_bolas(consecutivasDiagonal,tablero)
      
    }
    else //si no se ha conseguido ninguna linea de 5 nueva
    {
      llenar_tablero(0,3,tablero)  //se insertan 3 bolas nuevas en posiciones aleatorias
    }
     
  }
 
  def mejor_movimiento_tablero(fila: Int, columna: Int, color: Char, tablero: List[List[Char]], mejorFila: Int, mejorColumna: Int, tamano: Int ): List[Int] = 
  { //

    if(columna < 8)
    {
      
      val columna_aux = columna +1
      val consecutivasCol = get_bolas_columna(fila,columna_aux,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
      val consecutivasFila =  get_bolas_fila(fila, columna_aux, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
      val consecutivasDiagonal = get_bolas_diagonal(fila, columna_aux, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
      val mejor_lista= comparador_listas(consecutivasCol, consecutivasFila, consecutivasDiagonal)
      val mejor_tamano = mejor_lista.length
      if(mejor_tamano > tamano)
      {
         mejor_movimiento_tablero(fila, columna_aux, color, tablero, fila, columna_aux, mejor_tamano)
      
      }
      else
      {
         mejor_movimiento_tablero(fila, columna_aux, color, tablero, fila, columna, tamano)
      }
    }
    else
    {
      if(fila<8)
      {
        val fila_aux = fila+1
        val consecutivasCol = get_bolas_columna(fila_aux,0,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
        val consecutivasFila =  get_bolas_fila(fila_aux,0, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
        val consecutivasDiagonal = get_bolas_diagonal(fila_aux, 0, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
        val mejor_lista= comparador_listas(consecutivasCol, consecutivasFila, consecutivasDiagonal)
        val mejor_tamano = mejor_lista.length
        if(mejor_tamano > tamano)
        {
          mejor_movimiento_tablero(fila_aux, -1, color, tablero, fila_aux, 0, mejor_tamano)
        }
        else
        {
         mejor_movimiento_tablero(fila_aux, -1, color, tablero, fila, columna, tamano)
        }
      }
      else
      {
        val kaka = List(fila,columna,tamano) //QUITARRRRRRRR--------
        print(kaka)
        kaka
      }
    }

  }
  
   def estrategia(tablero: List[List[Char]], lista_colores: List[Char],fila: Int, columna: Int, tamano: Int, color: Char ): List[Any]=
  {
     if(!lista_colores.isEmpty)
     {
       println("FILA: "+fila)
       println("COLUMNA: "+columna)
       println("COLOR: "+color)
       val nuevo_color = lista_colores.head
       val lista = mejor_movimiento_tablero(0, -1, nuevo_color, tablero, 0, 0, 0)
       val mejor_fila = lista(0)
       val mejor_columna = lista(1)
       val tam = lista(2)
       if(tam > tamano)
       {
         estrategia(tablero, lista_colores.tail, mejor_fila, mejor_columna, tam, nuevo_color)
       }
       else
       {
         estrategia(tablero, lista_colores.tail, fila, columna, tamano, color)
       }
     }
     else
     {
       List(fila, columna, color)
     }

  }
  
  def comparador_listas(lista1: List[List[Int]], lista2: List[List[Int]],lista3: List[List[Int]]): List[List[Int]] = //compara la longitud de 3 listas y devuelve la mas grande
  {
    if(lista1.length > lista2.length)
    {
      if(lista1.length > lista3.length)
      {
        lista1 //1 es la mas grande
      }
      else //si la 3 es mas grande que la 1
      {
        if(lista3.length > lista2.length) //la 3 es la mas grande
        {
          lista3
        }
        else
        {
          lista1
        }
      }
    } //la 2 es mas grande que la 1
    else
    {
      if(lista2.length > lista3.length) //si la dos tambien es mas grande que la 3
      {
        lista2
      }
      else
      {
        lista3
      }
    }
  }

  
  def get_puntos(fila: Int, columna: Int, color: Char, tablero: List[List[Char]]): Int =
  { //obtiene los puntos en funcion de las bolas eliminadas
     val consecutivasCol = get_bolas_columna(fila,columna,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
    val consecutivasFila =  get_bolas_fila(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
    val consecutivasDiagonal = get_bolas_diagonal(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
    
    if(consecutivasCol.length >= 5) //si hay mas de 5 bolas 
    { 
      val puntos = consecutivasCol.length * 75 //multiplicamos el numero de bolas eliminadas por 75
      puntos

    }
    else if(consecutivasFila.length >= 5)
    { 
      val puntos = consecutivasFila.length * 75
      puntos
    }
    else if(consecutivasDiagonal.length >= 5)
    {
      val puntos = consecutivasDiagonal.length * 75
      puntos
    }
    else //si no se ha conseguido ninguna linea de 5 nueva
    {
      0 //supone 0 puntos ganados
    }
     
  }
}