--1.1 Celdas con bolitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}


data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

celdaNueva :: Celda
celdaNueva = Bolita Rojo (Bolita Azul CeldaVacia)

--1
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas col (Bolita c cel) = unoSi (sonDelMismoColor col c) + nroBolitas col cel

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

sonDelMismoColor :: Color -> Color -> Bool
sonDelMismoColor Rojo Rojo = True
sonDelMismoColor Azul Azul = True
sonDelMismoColor _ _ = False

--2
poner :: Color -> Celda -> Celda
poner col cel = Bolita col (cel)

--3
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar col (Bolita c cel) = if (sonDelMismoColor col c)
    then cel
    else (Bolita c (sacar col cel))

--4
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ x = x
ponerN n c cel = poner c (ponerN (n-1) c cel)


--1.2 Camino hacia el tesoro

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

camino :: Camino
camino = Nada (Cofre [Tesoro] (Cofre [Tesoro,Tesoro,Cacharro] (Nada Fin)))

--1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre o c) = hayTesoroAca o || hayTesoro c

hayTesoroAca :: [Objeto] -> Bool
hayTesoroAca [] = False
hayTesoroAca (x:xs) = esTesoro x || hayTesoroAca xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2
pasosHastaTesoro :: Camino -> Int
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = error "No hay tesoro en el camino"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre o c) = if hayTesoroAca o
    then 0
    else 1 + pasosHastaTesoro c


--3
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n c = hayCofreAca (habitacionA n c) && hayTesoroAca (objetosDe (habitacionA n c))

hayCofreAca :: Camino -> Bool
hayCofreAca (Cofre _ _) = True
hayCofreAca _ = False

objetosDe :: Camino -> [Objeto]
objetosDe (Cofre o c) = o

habitacionA :: Int -> Camino -> Camino
habitacionA _ Fin = Fin
habitacionA 0 x = x
habitacionA n (Nada c) = habitacionA (n - 1) c
habitacionA n (Cofre o c) = habitacionA (n - 1) c

--4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = (cantidadDeTesorosEn c) >= n

cantidadDeTesorosEn :: Camino -> Int
cantidadDeTesorosEn Fin = 0
cantidadDeTesorosEn (Nada c) = cantidadDeTesorosEn c
cantidadDeTesorosEn (Cofre o c) = cantTesorosEnCofre o + cantidadDeTesorosEn c

cantTesorosEnCofre :: [Objeto] -> Int
cantTesorosEnCofre [] = 0
cantTesorosEnCofre (x:xs) = if esTesoro x
    then 1 + cantTesorosEnCofre xs
    else cantTesorosEnCofre xs

--5
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2 c = (cantidadDeTesorosEn (habitacionA n1 c)) - (cantidadDeTesorosEn (habitacionA n2 c))





{-
lista :: [a] -> ...
lista [] = ...
lista (x:xs) = ... x ... lista xs

fun :: Camino -> ...
fun Fin = ...
fun c = ... c ... a c
-}
--Arboles
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol :: Tree Int
arbol = NodeT 1
                  (NodeT 4
                          (NodeT 2
                                   (NodeT 20 EmptyT EmptyT)
                                   (NodeT 10 EmptyT EmptyT))
                            EmptyT)
                  (NodeT 5
                          (NodeT 12 EmptyT EmptyT)
                          (NodeT 11 EmptyT (
                                           NodeT 20 EmptyT EmptyT)))


{-(NodeT 4 
                          (NodeT 2 EmptyT EmptyT)
                          (NodeT 10 EmptyT EmptyT))-}

--1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2


elementosDeArbol :: Tree a -> [a]
elementosDeArbol EmptyT = []
elementosDeArbol (NodeT a t1 t2) = a : elementosDeArbol t1 ++ elementosDeArbol t2

--2
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2



--3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x t1 t2) = (NodeT (x*2) (mapDobleT t1) (mapDobleT t2))


--4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT y (NodeT x t1 t2) = y == x || (perteneceT y t1 || perteneceT y t2)


--5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT = 0
aparicionesT y (NodeT x t1 t2) = if y == x
    then 1 + aparicionesT y t1 + aparicionesT y t2
    else aparicionesT y t1 + aparicionesT y t2



--6
leaves :: Tree a -> [a] 
leaves EmptyT = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT a t1 t2) = leaves t1 ++ leaves t2


--7
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = maxDelPar (1 + heightT t1, 1 + heightT t2)

maxDelPar :: (Int,Int) -> Int
maxDelPar (a, b) =
    if a > b
        then a
        else b


--8
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a t1 t2) = NodeT a (mirrorT t2) (mirrorT t1)

--9
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT a t1 t2) = reversa (elementosDeArbol t1) ++ [a] ++ elementosDeArbol t2

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (x:xs) y = x: agregarAlFinal xs y

--10
levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT a t1 t2) = [a]
levelN _ EmptyT = []
levelN n (NodeT a t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2



--11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1)  (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
--Tenemos que asumir que las ramas pueden medir diferente
juntarNiveles xs [] = xs
juntarNiveles [] ys = ys
juntarNiveles (x:xs) (y:ys) = (x++y) : juntarNiveles xs ys


--12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = if longitud (ramaMasLarga t1) > longitud (ramaMasLarga t2)
    then x : ramaMasLarga t1
    else x : ramaMasLarga t2



longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) =
     [x] : sucesionesDesde x (todosLosCaminos t1)
     ++ sucesionesDesde x (todosLosCaminos t2)

sucesionesDesde :: a -> [[a]] -> [[a]]
sucesionesDesde x [] = []
sucesionesDesde e (xs:xss) = [cons e xs] ++ sucesionesDesde e xss

cons :: a -> [a] -> [a]
cons x [] = [x]
cons e (xs) = (e:xs)



--expresiones aritméticas
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

operacion :: ExpA
operacion = Prod (Valor 3) (Valor 0)

--1
eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum x y) = eval x + eval y
eval (Prod x y) = eval x * eval y
eval (Neg x) = eval x * (-1)

--2
simplificar :: ExpA -> ExpA
simplificar (Sum x y) = simSuma x y
simplificar (Prod x y) = simProd x y
simplificar (Neg x) = simNeg x
simplificar (Valor n) = Valor n

simSuma :: ExpA -> ExpA -> ExpA
simSuma x (Valor 0) = x
simSuma (Valor 0) x = x
simSuma x y = Sum x y


simProd :: ExpA -> ExpA -> ExpA
simProd x (Valor 0) = Valor 0
simProd (Valor 0) x = Valor 0
simProd x (Valor 1) = x
simProd (Valor 1) x = x
simProd x y = Prod x y

simNeg :: ExpA -> ExpA
simNeg (Neg x) = x
simNeg x = Neg x


{-
--Recprdar esta estructura para iniciar
listPerLevel :: Tree a -> [[a]]
listPerLevel = EmptyT =
listPerLevel (NodeT x t1 t2) = ... x ... listPerLevel t1 ... listPerLevel t2 -}
