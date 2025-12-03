-- Arbol N-ario
data ArbolN a = Void | Node a [ArbolN a]
                deriving (Show, Eq)

-- Proposiciones logicas
data Prop = Var String |
            Cons Bool |
            Not Prop |
            And Prop Prop |
            Or Prop Prop |
            Impl Prop Prop |
            Syss Prop Prop
            deriving (Eq)

-- Para que se vea bien en la terminal
instance Show Prop where
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Algunas variables para no estar escribiendo mucho en la terminal
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Alias de estado y arbol de sintaxis
type Estado = [String]
type ArbolSintax = ArbolN String



--------------------------------------------------
--   SECCION 1: ARBOLES DE SINTAXIS ABSTRACTA   --
--------------------------------------------------

-- Funcion que convierte una proposicion logica a su arbol de sintaxis
propToArbol :: Prop -> ArbolSintax
propToArbol (Var p) = Node p []
propToArbol (Cons True) = Node "True" []
propToArbol (Cons False) = Node "False" []
propToArbol (Not p) = Node "¬" [propToArbol p]
propToArbol (Or p q) =  Node "∨" [propToArbol p, propToArbol  q]
propToArbol (And p q) = Node "∧" [propToArbol p, propToArbol q]
propToArbol (Impl p q) = Node "→" [propToArbol p, propToArbol q]
propToArbol (Syss p q) = Node "↔" [propToArbol p, propToArbol q]

-- Funcion que recibe un arbol de sintaxis y devuelve una proposicion
arbolToProp :: ArbolSintax -> Prop
arbolToProp Void = error "Arbol vacio"
arbolToProp (Node "True" []) = Cons True
arbolToProp (Node "False" []) = Cons False
arbolToProp (Node p []) = Var p
arbolToProp (Node "¬" [p]) = Not (arbolToProp p)
arbolToProp (Node "∨" [p, q]) = Or (arbolToProp p) (arbolToProp q)
arbolToProp (Node "∧" [p, q]) = And (arbolToProp p) (arbolToProp q)
arbolToProp (Node "→" [p, q]) = Impl (arbolToProp p) (arbolToProp q)
arbolToProp (Node "↔" [p, q]) = Syss (arbolToProp p) (arbolToProp q)

-- Funcion que recibe un arbol de sintaxis, un estado de variables y devuelve la interpretacion del arbol
interpretacionArbol :: ArbolSintax -> Estado ->  Bool
interpretacionArbol (Node "True" []) _ = True
interpretacionArbol (Node "False" []) _ = False
interpretacionArbol (Node p []) xs  =  if esElemento p xs then True else False
interpretacionArbol (Node "¬" [p]) xs = negacion (interpretacionArbol p xs)
interpretacionArbol (Node "∨" [p, q]) xs = disyuncion (interpretacionArbol p xs) (interpretacionArbol q xs)
interpretacionArbol (Node "∧" [p, q]) xs = conjuncion (interpretacionArbol p xs) (interpretacionArbol q xs)
interpretacionArbol (Node "→" [p, q]) xs = implicacion (interpretacionArbol p xs) (interpretacionArbol q xs)
interpretacionArbol (Node "↔" [p, q]) xs = bicondicional (interpretacionArbol p xs) (interpretacionArbol q xs)



------------------------------------
--   SECCION 2: OTRAS FUNCIONES   --
------------------------------------

numeroElementos :: ArbolN a -> Int
numeroElementos Void = 0
numeroElementos (Node tx hijos) = 1 + contarHijos hijos

--Función auxiliar para: numeroElementos
--Cuenta cuantos hijos tiene una lista
contarHijos :: [ArbolN a] -> Int
contarHijos [] = 0
contarHijos (t:ts) = numeroElementos t + contarHijos ts
--para que sume todos los hijos de una lista trees [tree,tree..]
-- contarHijos [t1,t2,t3] = numeroElementos t1 + numeroElementos t2 + numeroElementos t3

{- -- Mi implementacion de numeroElementos (iago)
-- Funcion que cuenta todos los elementos de un arbol n-ario
cantidadElementos :: ArbolN a -> Int
cantidadElementos Void = 0
cantidadElementos (Node _ lista) = 1 + cantidadElementosHijos lista
    where
        cantidadElementosHijos :: [ArbolN a] -> Int
        cantidadElementosHijos [] = 0
        cantidadElementosHijos (x:xs) = cantidadElementos x + cantidadElementosHijos xs
-}

busca :: Eq a => ArbolN a -> a -> Bool
busca Void _ = False
busca (Node tx hijos) a = if(tx==a) then True
                          else buscaHijos hijos a

--Función auxiliar para: busca
--Busca en la lista de hijos a ver si en los hijos se encuentra el elemento
buscaHijos :: Eq a => [ArbolN a] -> a -> Bool
buscaHijos [] _ = False
buscaHijos (Void:ts) a = buscaHijos ts a
buscaHijos ((Node x hijos):ts) a = if x == a then True
                                   else buscaHijos hijos a || buscaHijos ts a
                                   --para que busque en la lista de hijos de una rama y en los que estoy parado.
--deseo saber el valor a (x) del arbol actual, por lo tanto especifico (Node x hijos) : ts que es cada elemento

{- Mi implementacion de busca (iago)
-- Funcion que busca si un elemento pertenece a un arbol n-ario
busca :: Eq a => a -> ArbolN a -> Bool
busca _ Void = False
busca a (Node b lista) = if (a == b) then True else buscaHijos a lista
    where
        buscaHijos :: Eq a => a -> [ArbolN a] -> Bool
        buscaHijos _ [] = False
        buscaHijos a (x:xs) = disyuncion (busca a x) (buscaHijos a xs)
-}

sumaElementos :: Num a => ArbolN a -> a
sumaElementos Void = 0
sumaElementos (Node tx hijos) = tx + sumarHijos hijos

--Función auxiliar para: sumaElementos
--Obtiene la suma de los nodos hijos
sumarHijos :: Num a => [ArbolN a] -> a
sumarHijos [] = 0
sumarHijos (t:ts) = sumaElementos t + sumarHijos ts

{- Mi implementacion de sumaElementos (iago)
-- Funcion que toma un arbol n-ario de enteros y devuelve la suma de sus elementos
sumaElementos :: ArbolN Int -> Int
sumaElementos Void = 0
sumaElementos (Node x lista) = x + sumaElementosHijos lista
    where
        sumaElementosHijos :: [ArbolN Int] -> Int
        sumaElementosHijos [] = 0
        sumaElementosHijos (x:xs) = sumaElementos x + sumaElementosHijos xs
-}

preorden :: ArbolN a -> [a]
preorden Void = []
preorden (Node tx hijos) = [tx]++(recorrepreorden hijos)

--Función auxiliar para: preorden
--Agrega los recorridos de una lista de hijos a la lista de recorrido agregando primero los de la izquierda
recorrepreorden :: [ArbolN a] -> [a]
recorrepreorden [] = []
recorrepreorden (t:ts) = (preorden t)++(recorrepreorden ts)

{- Mi implementacion de preorden (iago)
-- Funcion que toma un arbol n-ario y regresa una lista de sus elementos, empezando por las ramas izquierdas y luego por las derechas
preorden :: ArbolN a -> [a]
preorden Void = []
preorden (Node a []) = [a]
preorden (Node a (x:xs)) = [a] ++ preorden x ++ preordenHijos xs
    where
        preordenHijos :: [ArbolN a] -> [a]
        preordenHijos [] = []
        preordenHijos (x:xs) = preorden x ++ preordenHijos xs
-}

-- Funcion que toma un arbol n-ario y regresa una lista de sus elementos, empezando por las ramas derechas y luego por las izquierdas
postorden :: ArbolN a -> [a]
postorden Void = []
postorden (Node a []) = [a]
postorden (Node a (x:xs)) = [a] ++ postordenHijos xs ++ postorden x
    where
        postordenHijos :: [ArbolN a] -> [a]
        postordenHijos [] = []
        postordenHijos (x:xs) = postordenHijos xs ++ postorden x

altura :: ArbolN a -> Int
altura Void = 0
altura (Node tx hijos) = 1 + alturaHijos hijos

--Función auxiliar para: altura
--Obtiene el máximo de las alturas de una lista de hijos, ya sea en la que estoy parado u otro en la lista
alturaHijos :: [ArbolN a] -> Int
alturaHijos [] = 0
alturaHijos (t:ts) = maximo (altura t)(alturaHijos ts)

{- Mi implementacion de altura (iago)
-- Funcion que toma un arbol n-ario y regresa la altura de dicho arbol
altura :: ArbolN a -> Int
altura Void = 0
altura (Node a []) = 1
altura (Node a (x:xs)) = maximo (1 + altura x) (1 + alturaHijos xs)
    where
        alturaHijos :: [ArbolN a] -> Int
        alturaHijos [] = 0
        alturaHijos (x:xs) = maximo (altura x) (alturaHijos xs)
-}

espejo :: ArbolN a -> ArbolN a --ya que no cambia el tipo de dato
espejo Void = Void
espejo (Node tx hijos) = (Node tx (invierte hijos))

--Función auxiliar para: espejo
--Invierte una lista de arboles (Los hijos de cada nodo padre)
invierte :: [ArbolN a] -> [ArbolN a]
invierte [] = []
invierte (t:ts) = snoc (invierte(ts)) (espejo t)

{- Mi implementacion de espejo (iago)
-- Funcion que invierte las ramas de un arbol n-ario. Las de la izquierda van a la derecha y viceversa
espejo :: ArbolN a -> ArbolN a
espejo Void = Void
espejo (Node a []) = (Node a [])
espejo (Node a lista) = (Node a (espejoHijos lista))
    where
        espejoHijos :: [ArbolN a] -> [ArbolN a]
        espejoHijos [] = []
        espejoHijos (x:xs) = espejoHijos xs ++ [espejo x]
-}

-- Funcion que corta ramas de un arbol
podar :: Int -> ArbolN a -> ArbolN a
podar 0 _ = Void
podar _ Void = Void
podar _ (Node a []) = (Node a []) -- El numero no puede ser 0 por la primera regla, entonces tiene que ser minimo 1. Y como este arbol solo tiene 1 nivel, el resultado es el mismo arbol
podar y (Node a (x:xs)) = (Node a ([podar (y-1) x] ++ (podarHijos (y-1) xs)))
    where
        podarHijos :: Int -> [ArbolN a] -> [ArbolN a]
        podarHijos 0 _ = []
        podarHijos _ [] = []
        podarHijos y (x:xs) = [podar (y-1) x] ++ (podarHijos (y-1) xs)

-- Funcion que devuelve la lista de elementos en una profundidad x de un arbol n-ario
elementosProfundidad :: Int -> ArbolN a -> [a]
elementosProfundidad _ Void = []
elementosProfundidad 0 (Node a _) = [a]
elementosProfundidad y (Node _ lista) = elementosProfundidadHijos (y-1) lista
    where
        elementosProfundidadHijos :: Int -> [ArbolN a] -> [a]
        elementosProfundidadHijos _ [] = []
        elementosProfundidadHijos 0 (x:xs) = (elementosProfundidad 0 x) ++ (elementosProfundidadHijos 0 xs)
        elementosProfundidadHijos y (x:xs) = (elementosProfundidad (y) x) ++ (elementosProfundidadHijos (y) xs)

------------------------------
--   FUNCIONES AUXILIARES   --
------------------------------

--Funcion maximo
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

--Funcion snoc
snoc :: [a] -> a -> [a]
snoc [] b = [b]
snoc (x:xs) b = x:(snoc xs b)

disyuncion :: Bool -> Bool -> Bool
disyuncion False False = False
disyuncion _ True = True
disyuncion True _ = True

negacion :: Bool -> Bool
negacion True = False
negacion False = True

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ False = False
conjuncion False _ = False

implicacion :: Bool -> Bool -> Bool
implicacion True True = True
implicacion False _ = True
implicacion True False = False

bicondicional :: Bool -> Bool -> Bool
bicondicional True True = True
bicondicional False False = True
bicondicional True False = False
bicondicional False True = False

esElemento :: Eq a => a -> [a] -> Bool --Funcion esElemento devuelve True si x es elemento de lista xs, False si no es elemento
esElemento _ [] = False
esElemento x (y:xs) = if (x == y) then True else (esElemento x xs)


-----------------
--   ARBOLES   --
-----------------

{-
t1 :: ArbolN Int
t1 = Node 1 [Node 2 [], Node 3 []]

t2 :: ArbolN Int
t2 = Node 10 [Void, Node 20 [Node 30 []], Void]

t3 :: ArbolN Int
t3 = Node 1 [
        Node 2 [
            Node 3 [
                Node 4 [
                    Node 5 []
                ]
            ]
        ]
    ]
t4 :: ArbolN Int
t4 = Node 0 [
        Node 1 [],
        Node 2 [],
        Node 3 [],
        Node 4 [],
        Node 5 [],
        Node 6 [],
        Node 7 [],
        Node 8 [],
        Node 9 []
    ]
t5 :: ArbolN Int
t5 = Node 100 [
        Node 50 [
            Node 20 [],
            Void,
            Node 30 [
                Node 15 [],
                Node 25 []
            ]
        ],
        Void,
        Node 200 [
            Node 150 [],
            Node 175 [
                Node 160 [],
                Void,
                Node 170 []
            ],
            Void
        ]
    ]
t6 :: ArbolN Int
t6 = Void

-- Árbol del diagrama:

t7 :: ArbolN Char
t7 =
  Node 'a'
    [ Node 'b'
        [ Node 'e'
            [ Node 'j' []
            , Node 'k' []
            ]
        ]
    , Node 'c' []
    , Node 'd'
        [ Node 'f' []
        , Node 'g' []
        , Node 'h' []
        , Node 'i' []
        ]
    ]
-}