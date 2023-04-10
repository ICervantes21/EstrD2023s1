--Ejercicio 1
--1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.ByteString (tails)
import Distribution.Simple.Utils (xargs)
import Text.XHtml (p, enctype)
import Prelude hiding (zip)

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
agregarAlFinal [] a = [a]
agregarAlFinal (x:xs) a = (x: agregarAlFinal xs a)

--12
agregar :: [a] -> [a] -> [a]
agregar [] a = a
agregar (x:xs) ys = x : agregar xs ys

--13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

--14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos n [] = n
zipMaximos [] n = n
zipMaximos (x:xs) (y:ys) = if x > y
    then x : zipMaximos xs ys
    else y : zipMaximos xs ys


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
cuentaRegresiva n = if n > 0
    then n: cuentaRegresiva (n - 1)
    else []

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
sinLosPrimeros 0 a = a
sinLosPrimeros n xs = if n < longitud xs
    then tail (sinLosPrimeros (n - 1) xs)
    else []


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


--Pokemon 2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Poke TipoDePokemon Int deriving Show
data Entrenador = Ent String [Pokemon] deriving Show

ash :: Entrenador
ash = Ent "ash" [charmander, mudkip, chicorita]

gary :: Entrenador
gary = Ent "gary" [chicorita]

charmander :: Pokemon
charmander = Poke Fuego 100

chicorita :: Pokemon
chicorita = Poke Planta 100

mudkip :: Pokemon
mudkip = Poke Agua 100

--1
cantPokemon :: Entrenador -> Int
cantPokemon (Ent n p) = longitud p

--2
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = pokemonesDeTipo t (pokemonesDe e)

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
pokemonesDeTipo _ [] = 0
pokemonesDeTipo t (x:xs) = if esDeTipo t x
    then 1 + pokemonesDeTipo t xs
    else pokemonesDeTipo t xs

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo Fuego (Poke Fuego e) = True
esDeTipo Agua (Poke Agua k) = True
esDeTipo Planta (Poke Planta j) = True
esDeTipo x z = False

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent n p) = p

--3
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = 
    if null (pokemonesDe e2)
        then longitud (pokemonesDe e1)
        else cuantosPokemonesLeGananATodos (soloLosDeTipo t (pokemonesDe e1)) (pokemonesDe e2)


superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke t1 n1) (Poke t2 n2) = esEfectivo t1 t2

esEfectivo :: TipoDePokemon -> TipoDePokemon -> Bool
esEfectivo Agua Fuego = True
esEfectivo Fuego Planta = True
esEfectivo Planta Agua = True
esEfectivo a b = False

cuantosPokemonesLeGananATodos :: [Pokemon] -> [Pokemon] -> Int
cuantosPokemonesLeGananATodos _ [] = 0
cuantosPokemonesLeGananATodos [] _ = 0
cuantosPokemonesLeGananATodos (x:xs) pokes = 
    if leGanaATodos x pokes
        then 1 + cuantosPokemonesLeGananATodos xs pokes
        else cuantosPokemonesLeGananATodos xs pokes

aCuantosLeGana :: Pokemon -> [Pokemon] -> Int 
aCuantosLeGana _ [] = 0    
aCuantosLeGana p (x:xs) = if superaA p x
    then 1 + aCuantosLeGana p xs
    else aCuantosLeGana p xs

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p pp = aCuantosLeGana p pp == longitud pp 

soloLosDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
soloLosDeTipo _ [] = []
soloLosDeTipo t (x:xs) = if esDeTipo t x
    then x: soloLosDeTipo t xs
    else soloLosDeTipo t xs

--4
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = poseePokemonDeTipo e Agua &&
    poseePokemonDeTipo e Fuego && poseePokemonDeTipo e Planta

poseePokemonDeTipo :: Entrenador -> TipoDePokemon -> Bool
poseePokemonDeTipo e t = hayUnPokemonDe_En_ t (pokemonesDe e)

hayUnPokemonDe_En_ :: TipoDePokemon -> [Pokemon] -> Bool
hayUnPokemonDe_En_ t pokes = not (null (soloLosDeTipo t pokes))


--Roles

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving (Show, Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

empresa :: Empresa
empresa = ConsEmpresa [Developer Junior proyecto1, Developer SemiSenior proyecto1, Developer Senior proyecto2]

proyecto1 :: Proyecto
proyecto1 = ConsProyecto "P1"

proyecto2 :: Proyecto
proyecto2 = ConsProyecto "P2"

--1
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa r) = sinRepetidos (proyectosDeRoles r)



proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (x:xs) = proyectosDe x: proyectosDeRoles xs

proyectosDe :: Rol -> Proyecto
proyectosDe (Developer _ x) = x
proyectosDe (Management _ y) = y

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if not (pertenece x xs)
    then x:sinRepetidos xs
    else sinRepetidos xs

--2
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa r) p = cuantosRolesDevSeniorEstanEn r p


cuantosRolesDevSeniorEstanEn :: [Rol] -> [Proyecto] -> Int
cuantosRolesDevSeniorEstanEn [] [] = 0
cuantosRolesDevSeniorEstanEn (x:xs) (y:ys) = if elRolEsDevSeniorYEstaEn x y
    then 1 + cuantosRolesDevSeniorEstanEn xs ys
    else cuantosRolesDevSeniorEstanEn xs ys


elRolEsDevSeniorYEstaEn :: Rol -> Proyecto -> Bool
elRolEsDevSeniorYEstaEn (Developer Senior p) x = p == x
elRolEsDevSeniorYEstaEn _ _ = False


--3
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn p (ConsEmpresa r) = contarCoincidenciasEntre p r

contarCoincidenciasEntre :: [Proyecto] -> [Rol] -> Int
contarCoincidenciasEntre [] [] = 0
contarCoincidenciasEntre (x:xs) (y:ys) = if elProyectoEstaEn x y 
    then 1 + contarCoincidenciasEntre xs ys
    else contarCoincidenciasEntre xs ys

elProyectoEstaEn :: Proyecto -> Rol -> Bool
elProyectoEstaEn p (Developer _ x) = p==x
elProyectoEstaEn p (Management _ x) = p==x


--4
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = proyectoXEmpleados (proyectos e) (rolesDe e) 


proyectoXEmpleados :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
proyectoXEmpleados _ [] = []
proyectoXEmpleados (x:xs) ys = (x, cuantosEmpleadosTiene x ys) : proyectoXEmpleados xs ys

cuantosEmpleadosTiene :: Proyecto -> [Rol] -> Int
cuantosEmpleadosTiene _ [] = 0
cuantosEmpleadosTiene p (x:xs) = unoSiTieneEmpleado p x + cuantosEmpleadosTiene p xs


unoSiTieneEmpleado :: Proyecto -> Rol -> Int
unoSiTieneEmpleado p r = if elProyectoEstaEn p r
    then 1
    else 0

rolesDe :: Empresa -> [Rol] 
rolesDe (ConsEmpresa r) = r    


zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

