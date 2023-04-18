import Main (caminoAlTesoro)
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
mapa = Bifurcacion (cofre2) (Fin (cofre1)) (Fin (cofre2))

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
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1
    then Izq : caminoAlTesoro m1
    else Der : caminoAlTesoro m2
