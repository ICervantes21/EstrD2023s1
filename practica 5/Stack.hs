module Stack

    (Stack, emptyS, isEmptyS, push, top, pop, lenS)

where

data Stack a = S [a] deriving Show



emptyS :: Stack a
emptyS = S []

--Crea una pila vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (S xs) = null xs

--Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)

--Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
top (S xs) = head xs

--Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
pop (S xs) = S (tail xs)

--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
lenS (S xs) = size xs

size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

--Dada la cantidad de elementos en la pila.