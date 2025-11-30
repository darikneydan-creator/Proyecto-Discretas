
    Hernández Bautista Iago
        No. de Cuenta: 323256223
    Omana Ramos Darik Neydan
        No. de Cuenta: 322211175
    Solorio Rauda Pedro
        No. de Cuenta: 323112343

_____________________________________________________________________
Tipo de dato usado:
Void | Node a [ArbolN a]

Este tipo de dato es un nodo con una lista de hijos, la razón de usar
este dato es porque queremos que cada nodo tenga cualquier número de hijos,
incluyendo la lista vacía, donde los hijos o elementos de la lista, también son
árboles n-arios, y además, no queremos que tenga dos listas (izq y der) 
como hijas, ya que esto no tendría ningún sentido, nos complicaría la lectura
y además podemos abarcar todos los hijos en una sola lista. Una desventaja de
este dato (probablemente, inevitable) es que nos forzó a elaborar funciones
auxiliares dentro y fuera de las principales, por el mismo cambio en el tipo de
dato que entraba y salía, una leía árboles, mientras que la otra leía arreglos
de árboles.

