--1.1 Celdas con bolitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

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
camino = Cofre [Tesoro] (Cofre [Tesoro] (Nada (Cofre [Tesoro] (Nada (Nada Fin)))))

--1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre o c) = if hayTesoroAca o 
    then True
    else hayTesoro c

hayTesoroAca :: [Objeto] -> Bool
hayTesoroAca [] = False
hayTesoroAca (x:xs) = if esTesoro x
    then True
    else hayTesoroAca xs

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
cantidadDeTesorosEn (Cofre o c) = unoSi (hayTesoroAca o) + cantidadDeTesorosEn c

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
--clase martes
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol :: Tree String
arbol = NodeT "a" 
                  (NodeT "b" EmptyT EmptyT)
                  (NodeT "d" EmptyT EmptyT)

{-
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel t = [elementosDeArbol t] 

elementosDeArbol :: Tree a -> [a]
elementosDeArbol EmptyT = []
elementosDeArbol (NodeT a t1 t2) = a ++ elementosDeArbol t1 ++ elementosDeArbol t2
-}
--Clase:

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1)  (listPerLevel t2)  

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] [] = []
--Tenemos que asumir que las ramas pueden medir diferente
juntarNiveles xs [] = xs
--juntarNiveler [] ys = ys
juntarNiveles (x:xs) (y:ys) = (x++y) : juntarNiveles xs ys

{-
--Recprdar esta estructura para iniciar
listPerLevel :: Tree a -> [[a]]
listPerLevel = EmptyT =
listPerLevel (NodeT x t1 t2) = ... x ... listPerLevel t1 ... listPerLevel t2 -}
