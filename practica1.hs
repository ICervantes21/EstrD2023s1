{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use camelCase" #-}
import Text.XHtml (p)

-- Ejercicio 1:
--A:

sucesor :: Int -> Int
sucesor a = a + 1


--B
sumar :: Int -> Int -> Int
sumar a b = a + b

--C
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (div a b, mod a b)

--D
maxDelPar :: (Int,Int) -> Int
maxDelPar (a, b) =
    if a > b
        then a
        else b

-- Ejercicio 2
-- sucesor (sumar (maxDelPar (divisionYResto 10 5)) 7)
-- sumar (maxDelPar (divisionYResto 25 5)) (sucesor 4)
-- sumar (sucesor (maxDelPar (divisionYResto  16 4))) 5
-- sucesor (sumar (maxDelPar (divisionYResto 36 6)) 3)

--Tipos enumerativos
-- Ejercicio 1:
data Dir  = Norte | Sur | Este | Oeste deriving Show

--A
opuesto :: Dir -> Dir
opuesto d =
    case d of
        Norte -> Sur
        Oeste -> Este
        Este -> Oeste
        Sur -> Norte

--B
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales a b = False

--C
siguiente :: Dir -> Dir
--Precondicion: Dir no puede ser "Oeste"
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe esa dirección"


--Ejercicio 2:
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

--A
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--B
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM d = False

--C
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues a b = codigoDelDia a > codigoDelDia b


codigoDelDia :: DiaDeSemana -> Int
codigoDelDia Lunes = 1
codigoDelDia Martes = 2
codigoDelDia Miercoles = 3
codigoDelDia Jueves = 4
codigoDelDia Viernes = 5
codigoDelDia Sabado = 6
codigoDelDia Domingo = 7

--D
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio d = True


-- Ejercicio 3:

--A
negar :: Bool -> Bool
negar True = False
negar _ = True

--B
implica :: Bool -> Bool -> Bool
implica True a = a
implica False a = True

--C
yTambien :: Bool -> Bool -> Bool
yTambien True a = a
yTambien False a = False

--D
oBien :: Bool -> Bool -> Bool
oBien True a = True
oBien False a = False


--Registros
--Ejercicio 1:
data Persona = P String Int deriving Show
               --Nombre Edad

yo :: Persona
yo = P "Ignacio" 26

otroYo :: Persona
otroYo = P "Nacho" 30

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (P b e) = P n e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P n2 e2) = e > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
    then p1
    else p2


--Pokemon
data TipoDePoke = Agua | Fuego | Planta deriving Show

data Pokemon = Poke TipoDePoke Int deriving Show

data Entrenador = Ent String Pokemon Pokemon

ash :: Entrenador
ash = Ent "ash" charmander chicorita

gary :: Entrenador
gary = Ent "gary" charmander charmander

charmander :: Pokemon
charmander = Poke Fuego 100

chicorita :: Pokemon
chicorita = Poke Planta 100

mudkip :: Pokemon
mudkip = Poke Agua 100

superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke t1 n1) (Poke t2 n2) = esEfectivo t1 t2

esEfectivo :: TipoDePoke -> TipoDePoke -> Bool
esEfectivo Agua Fuego = True
esEfectivo Fuego Planta = True
esEfectivo Planta Agua = True
esEfectivo a b = False

cantidadDePokemonDe :: TipoDePoke -> Entrenador -> Int
cantidadDePokemonDe t (Ent n p1 p2) =
    unoSi_CeroSino (esDeTipo t p1) + unoSi_CeroSino (esDeTipo t p2)


unoSi_CeroSino :: Bool -> Int
unoSi_CeroSino a = if a
    then 1
    else 0


esDeTipo :: TipoDePoke -> Pokemon -> Bool
esDeTipo Fuego (Poke Fuego e) = True
esDeTipo Agua (Poke Agua k) = True
esDeTipo Planta (Poke Planta j) = True
esDeTipo x z = False


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent n p1 p2) = [p1, p2]


--5: Funciones polimorficas

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- Las funciones son polimorficas porque les importa la estructura, pero no el tipo de dato

-- Ejercicio 6

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
--Precondición: La lista dada por el parámetro no es vacía
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
--Precondición: La lista dada por el parámetro no es vacía
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a, [a])
--Precondición: La lista dada por el parámetro no es vacía
splitHead (x:xs) = (x, xs)

