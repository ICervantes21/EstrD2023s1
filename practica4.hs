--Práctica 4

--Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza :: Pizza
pizza = Capa Queso (Capa Salsa (Capa (Aceitunas 4) (Capa Salsa Prepizza)))

pizza2 :: Pizza
pizza2 = Capa Queso (Capa Salsa (Capa (Aceitunas 4) Prepizza))

pizza3 :: Pizza
pizza3 = Capa Queso (Capa Salsa (Capa (Aceitunas 4) Prepizza))

pizza4 :: Pizza
pizza4 = Capa Queso (Capa Salsa (Capa (Aceitunas 4) Prepizza))

--1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--2
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa x p) = if esJamon x
    then sacarJamon p
    else Capa x (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False    

--4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa x p) = if esSalsaOQueso x
    then tieneSoloSalsaYQueso p
    else False

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

--5
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa x p) = Capa (duplicarSiSonAceitunas x) (duplicarAceitunas p)

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente 
duplicarSiSonAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarSiSonAceitunas i = i

--6
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = [(cantDeIngredientes x, x)] ++ cantCapasPorPizza xs

cantDeIngredientes :: Pizza -> Int
cantDeIngredientes Prepizza = 0
cantDeIngredientes (Capa x p) = 1 + cantDeIngredientes p


--Mapa de tesoros (con bifurcaciones)

{-Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro. -}

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

cofre1 :: Cofre
cofre1 = Cofre [Tesoro, Chatarra]

cofre2 :: Cofre
cofre2 = Cofre [Chatarra, Chatarra]

mapa :: Mapa
mapa = Bifurcacion (cofre2) (Fin (cofre1)) (
                            Bifurcacion (cofre2) (Fin (cofre1)) (Fin (cofre2)))

--1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnElCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnElCofre c || (hayTesoro m1 || hayTesoro m2)

hayTesoroEnElCofre :: Cofre -> Bool 
hayTesoroEnElCofre (Cofre xs) = hayTesoroEntreLosObjetos xs

hayTesoroEntreLosObjetos :: [Objeto] -> Bool
hayTesoroEntreLosObjetos [] = False
hayTesoroEntreLosObjetos (x:xs) = esTesoro x || hayTesoroEntreLosObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = hayTesoroEnElCofre c
hayTesoroEn [] (Bifurcacion c m1 m2) = hayTesoroEnElCofre c
hayTesoroEn (x:xs) (Fin c) = False
hayTesoroEn (x:xs) (Bifurcacion c m1 m2) = if esDerecha x
    then hayTesoroEn xs m2
    else hayTesoroEn xs m1

esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _ = False    
   

--3
caminoAlTesoro :: Mapa -> [Dir]
--Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1
    then Izq : caminoAlTesoro m1
    else Der : caminoAlTesoro m2


--4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = 
    if longitud (caminoDeLaRamaMasLarga m1) > longitud (caminoDeLaRamaMasLarga m2)
        then Izq : caminoDeLaRamaMasLarga m1
        else Der : caminoDeLaRamaMasLarga m2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesorosDelCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = 
    tesorosDelCofre c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
--Tenemos que asumir que las ramas pueden medir diferente
juntarNiveles xs [] = xs
juntarNiveles [] ys = ys
juntarNiveles (x:xs) (y:ys) = (x++y) : juntarNiveles xs ys


tesorosDelCofre :: Cofre -> [Objeto]
tesorosDelCofre (Cofre []) = []
tesorosDelCofre (Cofre (x:xs)) = if esTesoro x
    then x : tesorosDelCofre (Cofre xs)
    else tesorosDelCofre (Cofre xs)

--6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = []
todosLosCaminos (Bifurcacion c m1 m2) = 
    [Izq] : sucesionesDesde Izq (todosLosCaminos m1) ++ 
    [Der] : sucesionesDesde Der (todosLosCaminos m2)

sucesionesDesde :: a -> [[a]] -> [[a]]
sucesionesDesde x [] = []
sucesionesDesde e (xs:xss) = [cons e xs] ++ sucesionesDesde e xss    

cons :: a -> [a] -> [a]
cons x [] = [x]
cons e (xs) = (e:xs)

--3: Nave Espacial

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String 
type Tripulante = String 
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show