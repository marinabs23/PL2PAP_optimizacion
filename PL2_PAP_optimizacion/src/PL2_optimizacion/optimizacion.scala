package PL2_optimizacion

import java.io._

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
             
   val pw = new BufferedWriter(new FileWriter("puntos.txt", true));
   val bf = new BufferedReader(new FileReader("puntos.txt"))

  //Marina y Nicoleta
  def main(args: Array[String]) 
    {
    //MECANICA

    //Al comenzar la partida se llena el tablero con 9 bolas
    val tupla = llenar_tablero(0,9,tablero, 0)
    val tableroLleno = tupla._1
    println("Tablero inicial: ")
    imprimir_tablero(tableroLleno)
    //Se inicia el juego -> tupla._2 son los puntos iniciales 
    jugar(tableroLleno, tupla._2)
     
    }
  
   //Nicoleta y Marina
  def jugar(tablero: List[List[Char]], puntos: Int): List[List[Char]] =
  {
    if(esMiembro(tablero, 'O')) //Si quedan huecos libres
    { //Continua la partida
     val mov_recomendado: List[Any] = estrategia(tablero,colores,0,0,0,' ')
     val a = mov_recomendado(2).toString.toInt.toChar
     println("Se recomienda mover a la fila "+ mov_recomendado(0)+" columna "+mov_recomendado(1)+" una bola de color "+ a)
     val pos = elegirBola(tablero)
     val fila = pos(0)
     val columna = pos(1)
     val posNueva = elegirPosicion(tablero)
     val filaNueva = posNueva(0)
     val columnaNueva = posNueva(1)
     //Se mueve la bola a la posición indicada
     val tablero2 = mover_bola(fila, columna, filaNueva ,columnaNueva, tablero)
     val color = tablero2(filaNueva)(columnaNueva)
     val aux = actualizar_tablero(filaNueva, columnaNueva, color , tablero2, puntos) //comprobar si se ha hecho alguna fila columna o diagonal de 5 y eliminar bolas
     val tablero3 = aux._1
     val puntos2 = aux._2 
     //si no se ha hecho ninguna linea de 5 se meten 3 bolas aleatorias y se comprueba que con estas no se haya formado ninguna linea nueva
     val actualizado = bolasExtra(tablero2, tablero3, puntos2)
     val tablero4 = actualizado._1
     val puntos3 = actualizado._2
     imprimir_tablero(tablero4) 
     println("PUNTOS: "+ puntos3) 
     jugar(tablero4, puntos3) //se inicia otra ronda
         
     
    }
    else //Si no quedan huecos en el tablero se acaba la partida
    {
      println("Fin de la partida")
      println(tablero)
      println("1: SALIR")
      println("2: GUARDAR PUNTOS")
      println("3: REINICIAR")
      val opcion = scala.io.StdIn.readInt()
      opcion_final(opcion, puntos)
      tablero
    }
  }
   
   
  
    //Marina
  def bolasExtra(tablero1: List[List[Char]], tablero2: List[List[Char]], puntos: Int): (List[List[Char]], Int) = //comprueba si hay que añadir 3 bolas nuevas
  {
    if(tablero1 == tablero2) //si no se ha conseguido ninguna fila de 5
    {
      llenar_tablero(0,3,tablero1, puntos)
    }
    else //si no hay que añadirlas
    {
      (tablero2, puntos) //devuelve el tablero y los puntos como estaban
    }
  }
  
  //Nicoleta
 def opcion_final(opcion: Int, puntos: Int) //dependiendo de la opcion, cierra el juego, lo reinicia o guarda los puntos
  {
     opcion match {
       //salir
     case 1 => System.exit(0)
       //guardar
     case 2 => 
       //guardar puntos
       escribir(puntos)
       
      //imprimir ranking
       println("HISTORIAL DE RESULTADOS")
       imprimir_ranking(leer(Nil))
       //reiniciar
     case 3 => main(null)}
  }
  
  //Marina
  def imprimir_ranking(lista: List[Int])
  {
    if(lista.length != 0)
    {
      println(lista.head)
      imprimir_ranking(lista.tail)
    }
  }
  
   //Nicoleta
   def escribir(puntos: Int): Unit = {

    try
    {
      pw.write(puntos.toString) 
      pw.newLine()
    }
    finally pw.close()
   }
   
   //Marina
   def leer(lista: List[Int]): List[Int]={ //lee recursivamente un archivo linea a linea y devuelve las lineas en una lista
     
     val linea = bf.readLine()
     if(linea != null)
     {
       val lista2 = linea.toInt :: lista
       leer(lista2)
     }
     else
     {
        lista
     }

   }
   //Nicoleta
  def imprimir_tablero(tablero: List[List[Char]]){ //imprime el tablero con formato 
    
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
  //Nicoleta
  def imprimir_fila(fila: List[Char]){ //metodo auxiliar que imprime una fila
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
  
  //Marina
  def insertar_aux (color: Char, pos: Int, tablero: List[Char]): List[Char] = { //inserta un char en una lista
    if (pos==0) color::tablero.tail
    else tablero.head::insertar_aux(color, pos-1, tablero.tail)
  } 
     
  //Marina 
  def insertar_aux2 (lista: List[Char], pos: Int, tablero: List[List[Char]]): List[List[Char]] = { //inserta una lista en el tablero
    if (pos==0) lista::tablero.tail
    else tablero.head::insertar_aux2(lista, pos-1, tablero.tail)
  } 
  
  //Marina
  def insertar (color: Char, fila: Int, columna: Int, tablero: List[List[Char]]): List[List[Char]] = { 
    //actualiza el tablero dadas la fila y columna en las que se quiere insertar una bola y el color de esta
    val filaNueva:List[Char] = insertar_aux(color, columna, tablero(fila))
    insertar_aux2(filaNueva, fila, tablero) 
  }

   //Nicoleta y Marina
  def llenar_tablero(inicio: Int, numBolas: Int, tablero: List[List[Char]], puntos: Int): (List[List[Char]], Int) = { 
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
           val tupla = actualizar_tablero(fila, columna, bola, aux, puntos)
           val tablero2 = tupla._1
           val puntos2 = tupla._2
           llenar_tablero(inicio+1, numBolas, tablero2, puntos2) //llamada recursiva para seguir introduciendo bolas
         }
         else
         {
           llenar_tablero(inicio, numBolas, tablero, puntos)
         }
       }
       else
       {
         (tablero, puntos) //retorna el tablero 
       }
     }
     else
     {
       println("NO QUEDAN HUECOS")
       (tablero, puntos)
     }
   }
  
  
   //Marina
   def mover_bola(filaIni: Int, columnaIni: Int, filaFin: Int ,columnaFin: Int, tablero: List[List[Char]]): List[List[Char]] = { //mueve bola de (filaIni, columnaIni) a (filaFin, columnaFin)
     val bola = tablero(filaIni)(columnaIni) //obtener bola que se quiere mover
     val aux = insertar(bola, filaFin, columnaFin, tablero) //insertar bola adecuada en posicion final
     insertar('O', filaIni, columnaIni, aux)   //vaciar posicion inicial
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
  //Nicoleta
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
  
  //Nicoleta
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
  
    
   //Nicoleta y Marina
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
  //Nicoleta y Marina
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

   //Nicoleta y Marina 
   def get_bolas_fila(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = 
     //devuelve una lista con las fichas consecutivas del mismo color de una misma fila
   {
       val elementos_drch = elementos_fila_drch(fila,columna,color,tablero,Nil)
       val elementos_izq = elementos_fila_izq(fila,columna,color,tablero,Nil) 
       val posActual = List(fila, columna)
       val lista2 = posActual :: elementos_izq
       val listaConsecutivas = lista2 ++ elementos_drch
       listaConsecutivas
    }

    //Nicoleta y Marina
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
   //Nicoleta y Marina
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
    //Nicoleta y Marina
    def get_bolas_columna(fila: Int, columna: Int, color: Char,tablero: List[List[Char]],lista: List[List[Int]]):  List[List[Int]] = //devuelve una lista con las fichas consecutivas del mismo color de una misma columna
    {
       val elementos_abajo = elementos_columna_abajo(fila,columna,color,tablero,Nil)
       val elementos_arriba = elementos_columna_arriba(fila,columna,color,tablero,Nil)
       val posActual = List(fila, columna)
       val lista2 = posActual :: elementos_abajo
       val listaConsecutivas = lista2 ++ elementos_arriba
       listaConsecutivas
     }
    //Nicoleta
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
    //Nicoleta
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
    //Nicoleta
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
    //Marina
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
    //Nicoleta
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
    //Nicoleta
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
    
    //Marina
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
    
    //Marina
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
    
    //Marina
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
  //Marina
  def pos_en_linea(linea: List[List[Int]], pos: List[Int]): Boolean = //comprueba si una posicion corresponde a una linea
  {
    if(!linea.isEmpty)
    {
      val item = linea.head
      if(item == pos) //si el el primer elemento coincide con elem
      {
         true
      }
      else
      {
        pos_en_linea(linea.tail, pos) //comprueba la cola
      }
      
    }else
    {
      false
    }
  }
     
   //Marina
  def eliminar_bolas(lista: List[List[Int]],tablero: List[List[Char]], puntos: Int): (List[List[Char]], Int) =
    // dada una lista con las posiciones de bolas consecutivas del mismo color, sustituye dichas posiciones en el tablero por 0
  {
    if(!lista.isEmpty)
    {
      val posicion = lista.head //cogemos el primer elemento de la lista, cuya primera posicion es la fila y la segunda la columna
      val fila = posicion(0)
      val columna = posicion(1)
      val nuevo_tablero= insertar('O',fila,columna,tablero) //insertamos una O en esa posicion
      eliminar_bolas(lista.tail,nuevo_tablero, puntos+75) //llamamos con el resto de la lista
    }
    else
    {
      (tablero, puntos)
    }
   
  }
     
   //Marina y Nicoleta
  def actualizar_tablero(fila: Int, columna: Int, color: Char, tablero: List[List[Char]], puntos: Int):  (List[List[Char]], Int) =
  {//actualiza el tablero tras realizarse un movimiento
    val consecutivasCol = get_bolas_columna(fila,columna,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
    val consecutivasFila =  get_bolas_fila(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
    val consecutivasDiagonal = get_bolas_diagonal(fila, columna, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
    
    if(consecutivasCol.length >= 5) //si hay mas de 5 bolas 
    { 
      eliminar_bolas(consecutivasCol,tablero, puntos) //Se eliminan
    }
    else if(consecutivasFila.length >= 5)
    { 
      eliminar_bolas(consecutivasFila,tablero, puntos)
    }
    else if(consecutivasDiagonal.length >= 5)
    {
      eliminar_bolas(consecutivasDiagonal,tablero, puntos)
    }
    else
    {
          (tablero, puntos)
    }
  }
  
  //Marina
  def mejor_movimiento_tablero(fila: Int, columna: Int, color: Char, tablero: List[List[Char]], mejorFila: Int, mejorColumna: Int, tamano: Int ): List[Int] = 
  { //devuelve el mejor movimiento posible para una bola de un determinado color 


    if(columna < 8) //mientras que queden columnas de una fila por mirar
    {
      val columna_aux = columna +1 //se mira la siguiente columna
      val consecutivasCol = get_bolas_columna(fila,columna_aux,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
      val consecutivasFila =  get_bolas_fila(fila, columna_aux, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
      val consecutivasDiagonal = get_bolas_diagonal(fila, columna_aux, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
       
      val mejor_lista= comparador_listas(consecutivasCol, consecutivasFila, consecutivasDiagonal)
      val mejor_tamano = mejor_lista.length
      if(mejor_tamano > tamano) //si esta nueva posicion es mejor
      {
        if(tablero(fila)(columna_aux) == 'O')
        {
           mejor_movimiento_tablero(fila, columna_aux, color, tablero, fila, columna_aux, mejor_tamano) //llamada recursiva avanzando una casilla y actualizando valores maximos
      
        }
        else
        {
          mejor_movimiento_tablero(fila, columna_aux, color, tablero, mejorFila, mejorColumna, tamano) //mantiene valores maximos y avanza una casilla
        }
 
      }
      else //si no se ha mejorado el tamaño
      {
         mejor_movimiento_tablero(fila, columna_aux, color, tablero, mejorFila, mejorColumna, tamano) //mantiene valores maximos y avanza una casilla
      }
    }
    else //si se ha mirado ya una fila entera
    {
      if(fila<8)
      {
        val fila_aux = fila+1 //se pasa a la siguiente fila y se inicia desde la columna 0
        val consecutivasCol = get_bolas_columna(fila_aux,0,color,tablero,Nil) //obtenemos las bolas consecutivas que forman una columna a partir de la posicion indicada
        val consecutivasFila =  get_bolas_fila(fila_aux,0, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una fila a partir de la posicion indicada
        val consecutivasDiagonal = get_bolas_diagonal(fila_aux, 0, color, tablero, Nil) //obtenemos las bolas consecutivas que forman una diagonal a partir de la posicion indicada
        val mejor_lista= comparador_listas(consecutivasCol, consecutivasFila, consecutivasDiagonal)
        val mejor_tamano = mejor_lista.length
        if(mejor_tamano > tamano)
        {
          if(tablero(fila_aux)(columna) == 'O')
          {
            mejor_movimiento_tablero(fila_aux, -1, color, tablero, fila_aux, 0, mejor_tamano)
          }
          else
          {
            mejor_movimiento_tablero(fila_aux, -1, color, tablero, mejorFila, mejorColumna, tamano)
          }
          
        }
        else
        {
         mejor_movimiento_tablero(fila_aux, -1, color, tablero, mejorFila, mejorColumna, tamano)
        }
      }
      else //si ya ha recorrido todo el tablero
      {
       List(mejorFila,mejorColumna,tamano) //devuelve la mejor posicion y cantidad de bolas juntas del mismo color que se formarian
      
      }
    }

  }
  //Nicoleta
  def estrategia(tablero: List[List[Char]], lista_colores: List[Char],fila: Int, columna: Int, tamano: Int, color: Char ): List[Any]=
  {//determina el mejor movimento posible indicando en que casilla y de que color colocar la bola
     if(!lista_colores.isEmpty) 
     {
     
       val nuevo_color = lista_colores.head
       val lista = mejor_movimiento_tablero(0, -1, nuevo_color, tablero, 0, 0, 0)
      
       val mejor_fila = lista(0)
       val mejor_columna = lista(1)
       val tam = lista(2)
      
       
       if(tam > tamano) //compara el mejor movimiento posible para cada color (cantidad de bolas iguales juntas)
       { //si con el nuevo color es mejor que el anterior
         estrategia(tablero, lista_colores.tail, mejor_fila, mejor_columna, tam, nuevo_color) 
       }
       else //si no se ha mejorado el tamaño
       {
         estrategia(tablero, lista_colores.tail, fila, columna, tamano, color)
       }
     }
     else //si ha terminado de comparar todos los colores
     {
       
       val num_color = contar_fichas_color(tablero,color,0)
       if(num_color >= tamano)
       {
          List(fila, columna, color) //devuleve una lista con la fila, columna deonde se debe colocar la bola y el color de esta
          
       }
       else
       {
         //eliminar de la list de colores ese color ybuscar con los otros colores
         val nuevos_colores = eliminar_color(color,colores)
         estrategia(tablero,nuevos_colores,0,0,0,' ')
         
       }
      
     }

  }
  //Nicoleta
  def eliminar_aux(elem: Char, izq: List[Char], drch: List[Char]): List[Char] = //metodo auxliar para eliminar un elementod e una lista
   { 
     if (Nil == drch) //si la lsita de la drch esta vacia
     { 
        izq //devolvemos la lista de la izq
     } 
     if (elem == drch.head) //si el elemento es la cabeza de la lista de la drch
     { 
        izq ::: drch.tail //concatenamos la lista de la izq con el retso de la lista de la drch, de esta forma eliminamos el elemento
       
     } 
     eliminar_aux(elem, drch.head :: izq, drch.tail) //llamada recursiva
   } 
  
   def eliminar_color(elem: Char, lista: List[Char]):List[Char] = //elimina un elemeto de una lista de forma recursiva
   { 
      eliminar_aux(elem, Nil, lista) 
   }
  //Marina y Nicoleta 
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

  //Nicoleta
   def contar_fichas_color(tablero: List[List[Char]], color: Char, contador: Int): Int = 
    //cuenta las fichas de un color que hay en el tablero
  {
    if(!tablero.isEmpty) //mientras el tablero no este vacio
    {
      val fila = tablero.head //cogemos la primera fila y contamos las fichas del color dado
      val contador_aux = contar_fichas_color_fila(fila,color,0) + contador //actualizamos el valor
      contar_fichas_color(tablero.tail,color,contador_aux) //llamada recursiva con el resto de las filas del tablero
      
    }else
    {
      contador //devolvemos el contador
    }
    
  }
   //Nicoleta
  def contar_fichas_color_fila(fila: List[Char], color: Char, contador: Int): Int={
    //cuenta las bolas de un color de una fila
    if(!fila.isEmpty)
    {
      val elem = fila.head //cogemos el primer valor de la fila
    
      if(elem == color){ //en caso de que sea del color buscado
        val contador_aux = contador + 1 //acutualizamos el contador
        contar_fichas_color_fila(fila.tail,color,contador_aux) //llamada recursiva con el retso de elementos de la fila
      }
      else{ //si es distinto
        contar_fichas_color_fila(fila.tail,color,contador) //llamada recursiva con el contador sin actualiar
      }
      
    }
    else
    {
      contador //devolvemos el contador
    }
  }
}