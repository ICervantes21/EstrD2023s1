--Ejercicio 1
--1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.ByteString (tails)
import Distribution.Simple.Utils (xargs)
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



--Recursion sobre números

--1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

--2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n: cuentaRegresiva (n - 1)

--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e: repetir (n-1) e

--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x: losPrimeros (n - 1) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 [e] = [e]
sinLosPrimeros n (x:xs) = tail (x: sinLosPrimeros (n - 1) xs)


--Registros

data Persona = P String Int deriving Show

yo :: Persona
yo = P "Ignacio" 26

otroYo :: Persona
otroYo = P "Nacho" 30

--1
esMayorA :: Int -> Persona -> Bool
esMayorA n (P p e) = e >= n 

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if esMayorA n x
    then x : mayoresA n xs
    else mayoresA n xs

--2
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "La lista no puede ser vacía"
promedioEdad p = promedio (edadesDe p)

promedio :: [ Int ] -> Int
-- PRECOND: la lista no es vacía
promedio ns = div (sumatoria ns) (longitud ns)

edadesDe :: [Persona] -> [Int]
edadesDe [] = []
edadesDe (x:xs) = edad x: edadesDe xs

edad :: Persona -> Int
edad (P n e) = e


--3
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "La lista no debe ser vacía"
elMasViejo (x:xs) = if edad x == maximoDe (edadesDe (x:xs))
    then x
    else elMasViejo xs

