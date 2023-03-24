--Ejercicio 1
--1
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
pertenece e (x:xs) = x  pertenece e xs   -- ver video, usar if