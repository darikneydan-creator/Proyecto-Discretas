--Colocar aquí su árbol, o en la terminal

{- 
cantLista :: [a] -> Int
cantLista [] = 0
cantLista (x:xs) = 1 + cantLista xs 
-}
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

data ArbolN a = Void | Node a [ArbolN a] --nodo con lista de n hijos
                deriving (Show, Eq)


--Funciones auxiliares (hacen toda la chamba prácticamente)

--Función auxiliar para: numeroElementos
--Cuenta cuantos hijos tiene una lista
contarHijos :: [ArbolN a] -> Int
contarHijos [] = 0
contarHijos (t:ts) = numeroElementos t + contarHijos ts
--para que sume todos los hijos de una lista trees [tree,tree..]
-- contarHijos [t1,t2,t3] = numeroElementos t1 + numeroElementos t2 + numeroElementos t3

--Función auxiliar para: sumaElementos
--Obtiene la suma de los nodos hijos
sumarHijos :: Num a => [ArbolN a] -> a
sumarHijos [] = 0
sumarHijos (t:ts) = sumaElementos t + sumarHijos ts

--Función auxiliar para: busca
--Busca en la lista de hijos a ver si en los hijos se encuentra el elemento
buscaHijos :: Eq a => [ArbolN a] -> a -> Bool
buscaHijos [] _ = False
buscaHijos (Void:ts) a = buscaHijos ts a
buscaHijos ((Node x hijos):ts) a = if x == a then True
                                   else buscaHijos hijos a || buscaHijos ts a
                                   --para que busque en la lista de hijos de una rama y en los que estoy parado.
--deseo saber el valor a (x) del arbol actual, por lo tanto especifico (Node x hijos) : ts que es cada elemento

--Funcion maximo
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

--Funcion snoc
snoc :: [a] -> a -> [a]
snoc [] b = [b]
snoc (x:xs) b = x:(snoc xs b)

--Función auxiliar para: espejo
--Invierte una lista de arboles (Los hijos de cada nodo padre)
invierte :: [ArbolN a] -> [ArbolN a]
invierte [] = []
invierte (t:ts) = snoc (invierte(ts)) (espejo t) 

--Función auxiliar para: altura
--Obtiene el máximo de las alturas de una lista de hijos, ya sea en la que estoy parado u otro en la lista
alturaHijos :: [ArbolN a] -> Int
alturaHijos [] = 0
alturaHijos (t:ts) = maximo (altura t)(alturaHijos ts)

--Función auxiliar para: preorden
--Agrega los recorridos de una lista de hijos a la lista de recorrido agregando primero los de la izquierda
recorrepreorden :: [ArbolN a] -> [a]
recorrepreorden [] = []
recorrepreorden (t:ts) = (preorden t)++(recorrepreorden ts) 

-------------------------------------------------------------------------------

numeroElementos :: ArbolN a -> Int
numeroElementos Void = 0
numeroElementos (Node tx hijos) = 1 + contarHijos hijos

sumaElementos :: Num a => ArbolN a -> a
sumaElementos Void = 0
sumaElementos (Node tx hijos) = tx + sumarHijos hijos

busca :: Eq a => ArbolN a -> a -> Bool
busca Void _ = False
busca (Node tx hijos) a = if(tx==a) then True 
                          else buscaHijos hijos a

altura :: ArbolN a -> Int
altura Void = 0
altura (Node tx hijos) = 1 + alturaHijos hijos

espejo :: ArbolN a -> ArbolN a --ya que no cambia el tipo de dato
espejo Void = Void
espejo (Node tx hijos) = (Node tx (invierte hijos))

preorden :: ArbolN a -> [a]
preorden Void = []
preorden (Node tx hijos) = [tx]++(recorrepreorden hijos)