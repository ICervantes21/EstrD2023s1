module Set

   (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 

where

data Set a = S [a] deriving Show

emptyS :: Set a
emptyS = S []
--Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
addS x (S xs) = if not (belongs x (S xs))
    then (S (x : xs))
    else (S xs)
--Dados un elemento y un conjunto, agrega el elemento al conjunto.

sinRepetir :: Eq a => [a] -> [a]
sinRepetir [] = []
sinRepetir (x:xs) = if not (pertenece x xs)
    then x : sinRepetir xs
    else sinRepetir xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = x == e || pertenece e xs

 
belongs :: Eq a => a -> Set a -> Bool
belongs y (S xs) = pertenece y xs
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = size xs 
--Devuelve la cantidad de elementos distintos de un conjunto.

size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

removeS :: Eq a => a -> Set a -> Set a
removeS e (S xs) = (S (removerDe e xs))

removerDe ::Eq a => a -> [a] -> [a]
removerDe _ [] = []
removerDe e (x:xs) = if e == x
    then xs
    else x : removerDe e xs
--Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = (S (sinRepetir (union xs ys)))

union :: [a] -> [a] -> [a]
union xs ys = xs ++ ys
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.