
--1.1 Celdas con bolitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}


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
                  (NodeT 5 
                          (NodeT 10 EmptyT EmptyT)
                          (NodeT 10 EmptyT EmptyT))
                  (NodeT 5 
                          (NodeT 10 EmptyT EmptyT)
                          (NodeT 10 EmptyT EmptyT))

--1
sumarT :: Tree Int -> Int
sumarT t = sumatoria (elementosDeArbol t)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

elementosDeArbol :: Tree a -> [a]
elementosDeArbol EmptyT = []
elementosDeArbol (NodeT a t1 t2) = a : elementosDeArbol t1 ++ elementosDeArbol t2

--2
sizeT :: Tree a -> Int
sizeT t = longitud (elementosDeArbol t)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x t1 t2) = (NodeT (x*2) (mapDobleT t1) (mapDobleT t2))


--4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a t = pertenece a (elementosDeArbol t)


pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a t = apariciones a (elementosDeArbol t)

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if e == x
    then 1 + apariciones e xs
    else apariciones e xs

--6
leaves :: Tree a -> [a] --La había realizado sin querer, se llama "elementosDeARbol"
leaves EmptyT = []
leaves (NodeT a t1 t2) = a : elementosDeArbol t1 ++ elementosDeArbol t2

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
mirrorT (NodeT a t1 t2) = (NodeT a t2 t1)

--9
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT a t1 t2) = (leaves t1) ++ [a] ++ leaves t2

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
juntarNiveles [] [] = []
--Tenemos que asumir que las ramas pueden medir diferente
juntarNiveles xs [] = xs
juntarNiveles (x:xs) (y:ys) = (x++y) : juntarNiveles xs ys


--12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga (NodeT a t1 t2)= if sizeT t1 > sizeT t2
    then leaves t1
    else leaves t2

--13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a t1 t2) = ([leaves t1]++[leaves t2])


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
simProd x (Valor 0) = (Valor 0)
simProd (Valor 0) x = (Valor 0)
simProd x (Valor 1) = x
simProd (Valor 1) x = x
simProd x y = Prod x y

simNeg :: ExpA -> ExpA
simNeg (Neg x) = x
simNeg x = (Neg x)


{-
--Recprdar esta estructura para iniciar
listPerLevel :: Tree a -> [[a]]
listPerLevel = EmptyT =
listPerLevel (NodeT x t1 t2) = ... x ... listPerLevel t1 ... listPerLevel t2 -}
