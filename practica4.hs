--Práctica 4

--Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza :: Pizza
pizza = Capa Jamon (Capa Queso(Capa (Aceitunas 4) Prepizza))

--1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p