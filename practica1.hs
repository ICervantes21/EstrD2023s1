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


-- Ejercicio 3:
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


--Ejercicio 4:
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


-- Ejercicio 5:

--A
negar :: Bool -> Bool
negar a = not a

--B
implica :: Bool -> Bool -> Bool
implica True False = False
implica a b = True

--C
yTambien :: Bool -> Bool -> Bool