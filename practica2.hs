--Ejercicio 1
--1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x + 1: sucesores xs

--4
conjuncion :: [Bool] -> Bool
conjuncion [] = True 
conjuncion (x:xs) = x && conjuncion xs

--5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs 

--8
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if e == x
    then 1 + apariciones e xs
    else apariciones e xs 

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if x<n 
    then x : losMenoresA n xs
    else losMenoresA n xs

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Propósito: Dados un número n y una lista de listas, 
--devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
    then x : lasDeLongitudMayorA n xs
    else lasDeLongitudMayorA n xs

--11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = (x:xs)++[e]

--12
agregar :: [a] -> [a] -> [a]
agregar (x:xs) (y:ys) = (x:xs) ++ (y:ys)

--13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

--14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = [maximoDe xs, maximoDe ys]

maximoDe :: [Int] -> Int
--Próposito: Dada una lista de números enteros, retorna el maximo de esas lista.
--Precondicion: La lista dada no es vacía
maximoDe [n] = n
maximoDe (x:xs) = if x > maximoDe xs
    then x
    else maximoDe xs

--15
elMinimo :: Ord a => [a] -> a
--precondiciones: La lista dada no es vacía.
elMinimo [a] = a
elMinimo (x:xs) = if x < elMinimo xs
    then x
    else elMinimo xs
