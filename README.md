Implementamos los arboles n-arios de la siguiente forma:

```data ArbolN a = Void | Node a [ArbolN a]```

El tipo de dato que elegimos es un nodo con una lista de hijos.

Decidimos usar este tipo de dato porque las listas permiten que cada nodo tenga una cantidad arbitraria de arboles n-arios hijos.
Además de tener cualquier cantidad de hijos, las listas nos permiten conservar el orden de los nodos. No es lo mismo tener este arbol:
```
  A
 / \
B   C
   / \
  D   E
```
a tener este arbol:
```
    A
   / \
  C   B
 / \
E   D
```
Por si solo puede no parecer útil, pero sí importa cuando nos encontramos con funciones que requieren orden (como preorden y postorden).

Y la última razón por la que elegimos usar listas para manejar a los hijos es porque ya las habíamos visto en clase, entonces estamos más familiarizados con cómo funcionan y cómo usarlas.

Aunque usamos listas para hacer los árboles n-arios, también tienen desventajas. Por ejemplo, como los árboles pueden estar sueltos o en listas, nos forzó a crear funciones auxiliares dentro de cada función principal para tratar con árboles sueltos y para tratar con listas de árboles.

Además, como un árbol puede ser vacío, existen dos formas de representar un nodo sin hijos. Una forma es con una lista vacía

``` Node x [] ```

y otra forma es con una lista de arboles vacíos

``` Node x [Void] ```

No afecta a la hora de contar elementos o trabajar con las listas, porque una lista de árboles vacía y un árbol vacío son tratados casi igual, pero no es muy elegante tener dos formas de representar la misma cosa, puede causar ambigüedad y confusión

_____________________________________________________________________

# LISTA DE FUNCIONES:

## ÁRBOLES DE SINTAXIS ABSTRACTA:

- ```propToArbol :: Prop -> ArbolSintax```

    Función que convierte una proposición lógica a su árbol de sintaxis

- ```arbolToProp :: ArbolSintax -> Prop```

    Función que recibe un árbol de sintaxis y devuelve una proposición

- ```interpretacionArbol :: ArbolSintax -> Estado -> Bool```

    Función que recibe un árbol de sintaxis, un estado de variables y devuelve la interpretación del árbol


## OTRAS FUNCIONES:

- ```cantidadElementos :: ArbolN a -> Int```

    Función que cuenta todos los elementos de un árbol n-ario

- ```busca :: a -> ArbolN a -> Bool```

    Función que busca si un elemento pertenece a un árbol n-ario

- ```sumaElementos :: ArbolN Int -> Int```

    Función que toma un árbol n-ario de enteros y devuelve la suma de sus elementos

- ```preorden :: ArbolN a -> [a]```

    Función que toma un árbol n-ario y regresa una lista de sus elementos según los encuentra en recorrido en preorden

- ```postorden :: ArbolN a -> [a]```

    Función que toma un árbol n-ario y regresa una lista de sus elementos según los encuentra en recorrido en postorden

- ```altura :: ArbolN a -> Int```

    Función que toma un árbol n-ario y regresa la altura de dicho árbol

- ```espejo :: ArbolN a -> ArbolN a```

    Función que invierte las ramas de un árbol n-ario. Las de la izquierda van a la derecha y viceversa

- ```podar :: Int -> ArbolN a -> ArbolN a```

    Función que toma un nivel de profundidad y corta las ramas de un árbol a partir de ahí

- ```elementosProfundidad :: Int -> ArbolN a -> [a]```

    Función que devuelve la lista de elementos en una profundidad x de un árbol n-ario

_____________________________________________________________________

# Miembros del equipo:
- Hernández Bautista Iago

    No. de cuenta: ```323256223```

- Omana Ramos Darik Neydan

    No. de cuenta: ```322211175```

- Solorio Rauda Pedro

    No. de cuenta: ```323112343```

_____________________________________________________________________
# Info adicional:
Dentro de ```proyecto.hs``` hay varios arboles (t1 a t9) y proposiciones (p1 a p3) para que sea más fácil probar las funciones. Si descarga el repositorio, puede editar ```proyecto.hs``` para agregar, cambiar o eliminar las plantillas que tenemos.
