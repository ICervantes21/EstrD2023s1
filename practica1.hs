import Text.XHtml (p)

-- Ejercicio 1:
--A:

sucesor :: Int -> Int
sucesor a = a - 1


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
-- sucesor (sumar (maxDelPar (divisionYResto 10 5)) 9)
-- sumar (maxDelPar (divisionYResto 25 5)) (sucesor 6)
-- sumar (sucesor (maxDelPar (divisionYResto 49 7))) 4
-- sucesor (sumar (maxDelPar (divisionYResto 36 6)) 5)

--Tipos enumerativos
-- Ejercicio 1:
data Dir  = Norte | Sur | Este | Oeste deriving (Show, Eq)

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
negar a = not a

--B
implica :: Bool -> Bool -> Bool
implica True False = False
implica a b = True

--C
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien a b = False

--D
oBien :: Bool -> Bool -> Bool
oBien True False = True
oBien False True = True
oBien a b = False


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
laQueEsMayor (P n1 e1) (P n2 e2) = if esMayorQueLaOtra (P n1 e1) (P n2 e2)
    then P n1 e1
    else P n2 e2


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
cantidadDePokemonDe t (Ent n p1 p2)
  | sonDelMismoTipo t p1 p2 = 2
  | unoEsDeTipo t p1 p2 = 1
  | otherwise = 0


unoEsDeTipo :: TipoDePoke -> Pokemon -> Pokemon -> Bool
unoEsDeTipo t (Poke x e) (Poke z e2) =
     esDeTipo t (Poke x e) || esDeTipo t (Poke z e2)

sonDelMismoTipo :: TipoDePoke -> Pokemon -> Pokemon -> Bool
sonDelMismoTipo t (Poke x e) (Poke z e2) =
     esDeTipo t (Poke x e) && esDeTipo t (Poke z e2)

tipoDe :: Pokemon -> TipoDePoke
tipoDe (Poke t e) = t

esDeTipo :: TipoDePoke -> Pokemon -> Bool
esDeTipo Fuego (Poke Fuego e) = True
esDeTipo Agua (Poke Agua k) = True
esDeTipo Planta (Poke Planta j) = True
esDeTipo x z = False


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (Ent n p1 p2, Ent n2 p3 p4) = [p1, p2, p3, p4]


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
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (x:a) = a

splitHead :: [a] -> (a, [a])
splitHead (x:a) = (x, a)

