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




