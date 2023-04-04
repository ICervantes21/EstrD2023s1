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
camino = Cofre [Cacharro] (Nada (Nada (Nada (Nada (Nada Fin)))))

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
--hayTesoroEn :: Int -> Camino -> Bool

