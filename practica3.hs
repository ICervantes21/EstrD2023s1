--1.1 Celdas con bolitas

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

