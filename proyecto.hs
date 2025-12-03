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
interpretacionArbol :: ArbolSintax -> Estado -> Bool
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

-- Funcion que cuenta todos los elementos de un arbol n-ario
cantidadElementos :: ArbolN a -> Int
cantidadElementos Void = 0
cantidadElementos (Node _ hijos) = 1 + cantidadElementosHijos hijos
    where
        cantidadElementosHijos :: [ArbolN a] -> Int
        cantidadElementosHijos [] = 0
        cantidadElementosHijos (x:xs) = cantidadElementos x + cantidadElementosHijos xs

-- Funcion que busca si un elemento pertenece a un arbol n-ario
busca :: Eq a => a -> ArbolN a -> Bool
busca _ Void = False
busca a (Node b hijos) = if (a == b) then True else buscaHijos a hijos
    where
        buscaHijos :: Eq a => a -> [ArbolN a] -> Bool
        buscaHijos _ [] = False
        buscaHijos a (x:xs) = disyuncion (busca a x) (buscaHijos a xs)

-- Funcion que toma un arbol n-ario de enteros y devuelve la suma de sus elementos
sumaElementos :: ArbolN Int -> Int
sumaElementos Void = 0
sumaElementos (Node x hijos) = x + sumaElementosHijos hijos
    where
        sumaElementosHijos :: [ArbolN Int] -> Int
        sumaElementosHijos [] = 0
        sumaElementosHijos (x:xs) = sumaElementos x + sumaElementosHijos xs

-- Funcion que toma un arbol n-ario y regresa una lista de sus elementos segun los encuentra en recorrido en preorden
preorden :: ArbolN a -> [a]
preorden Void = []
preorden (Node a hijos) = [a] ++ preordenHijos hijos
    where
        preordenHijos :: [ArbolN a] -> [a]
        preordenHijos [] = []
        preordenHijos (x:xs) = preorden x ++ preordenHijos xs

-- Funcion que toma un arbol n-ario y regresa una lista de sus elementos segun los encuentra en recorrido en postorden

postorden :: ArbolN a -> [a]
postorden Void = []
postorden (Node a hijos) = postordenHijos hijos ++ [a]
    where
        postordenHijos :: [ArbolN a] -> [a]
        postordenHijos [] = []
        postordenHijos (x:xs) = postorden x ++ postordenHijos xs

-- Funcion que toma un arbol n-ario y regresa la altura de dicho arbol
altura :: ArbolN a -> Int
altura Void = 0
altura (Node a hijos) = 1 + alturaHijos hijos
    where
        alturaHijos :: [ArbolN a] -> Int
        alturaHijos [] = 0
        alturaHijos (x:xs) = maximo (altura x) (alturaHijos xs)

-- Funcion que invierte las ramas de un arbol n-ario. Las de la izquierda van a la derecha y viceversa
espejo :: ArbolN a -> ArbolN a
espejo Void = Void
espejo (Node a hijos) = (Node a (espejoHijos hijos))
    where
        espejoHijos :: [ArbolN a] -> [ArbolN a]
        espejoHijos [] = []
        espejoHijos (x:xs) = snoc (espejoHijos xs) (espejo x)

-- Funcion que toma un nivel de profundidad y corta las ramas de un arbol a partir de ahi
podar :: Int -> ArbolN a -> ArbolN a
podar _ Void = Void
podar 0 _ = Void
podar y (Node a hijos) = (Node a (podarHijos (y-1) hijos))
    where
        podarHijos :: Int -> [ArbolN a] -> [ArbolN a]
        podarHijos _ [] = []
        podarHijos 0 _ = []
        podarHijos y (x:xs) = [podar (y) x] ++ (podarHijos (y) xs)

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

-- Funcion maximo
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

-- Funcion snoc
snoc :: [a] -> a -> [a]
snoc [] b = [b]
snoc (x:xs) b = x:(snoc xs b)

-- Funcion disyuncion
disyuncion :: Bool -> Bool -> Bool
disyuncion False False = False
disyuncion _ True = True
disyuncion True _ = True

-- Funcion negacion
negacion :: Bool -> Bool
negacion True = False
negacion False = True

-- Funcion conjuncion
conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ False = False
conjuncion False _ = False

-- Funcion implicacion
implicacion :: Bool -> Bool -> Bool
implicacion True True = True
implicacion False _ = True
implicacion True False = False

-- Funcion bicondicional
bicondicional :: Bool -> Bool -> Bool
bicondicional True True = True
bicondicional False False = True
bicondicional True False = False
bicondicional False True = False

-- Funcion esElemento
esElemento :: Eq a => a -> [a] -> Bool --Funcion esElemento devuelve True si x es elemento de lista xs, False si no es elemento
esElemento _ [] = False
esElemento x (y:xs) = if (x == y) then True else (esElemento x xs)



---------------------------------
--   ARBOLES Y PROPOSICIONES   --
---------------------------------

t1 :: ArbolN Int
t1 = Node 1 [
        Node 2 [],
        Node 3 []
    ]

t2 :: ArbolN Int
t2 = Node 10 [
        Void,
        Node 20 [
            Node 30 []
        ],
        Void
    ]

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

t7 :: ArbolN Char
t7 = Node 'a' [
        Node 'b' [
            Node 'e' [
                Node 'j' [],
                Node 'k' []
            ]
        ],
        Node 'c' [],
        Node 'd' [
            Node 'f' [],
            Node 'g' [],
            Node 'h' [],
            Node 'i' []
        ]
    ]

t8 :: ArbolN Char
t8 = Node 'A' [
        Node 'B' [
            Node 'D' [
                Node 'H' [],
                Node 'I' []
            ],
            Node 'E' [
                Node 'J' [],
                Node 'K' []
            ]
        ],
        Node 'C' [
            Node 'F' [
                Node 'L' [],
                Node 'M' []
            ],
            Node 'G' [
                Node 'N' [],
                Node 'O' []
            ]
        ]
    ]

p1 :: Prop
p1 =
  Impl
    (And
       (Or (Var "p") (Not (Var "q")))
       (Impl (Var "r") (And (Var "s") (Not (Var "t"))))
    )
    (Syss
       (Or (Var "u") (Var "v"))
       (And (Not (Var "w")) (Impl (Var "x") (Var "y")))
    )

p2 :: Prop
p2 =
  And
    (Not (Impl (Var "a") (Not (Var "b"))))
    (Or
       (Impl
          (And (Var "c") (Var "d"))
          (Not (Or (Var "e") (Var "f")))
       )
       (Syss
          (Impl (Var "g") (Var "h"))
          (And (Not (Var "i")) (Not (Var "j")))
       )
    )

p3 :: Prop
p3 =
  Syss
    (And
       (Or (Var "p1") (Var "p2"))
       (And
          (Impl (Var "p3") (Or (Var "p4") (Var "p5")))
          (Not (And (Var "p6") (Var "p7")))
       )
    )
    (Impl
       (Or
          (And (Var "p8") (Not (Var "p9")))
          (Impl (Var "p10") (Var "p11"))
       )
       (And (Var "p12") (Not (Var "p13")))
    )


{-
Miembros de equipo:

Hernández Bautista Iago
    No. de cuenta: 323256223

Omana Ramos Darik Neydan
    No. de cuenta: 322211175

Solorio Rauda Pedro
    No. de cuenta: 323112343
-}
