import GHC.Float (int2Float)
import Distribution.Simple.Setup (trueArg)

doubleMe :: Int -> Int
doubleMe x = x + x

f :: Int -> Int
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16
    | otherwise  = error "elija un numero entre 1, 4 y 16"

g :: Int -> Int
g n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16
    | otherwise  = error "elija un numero entre 1, 4 y 16"

h :: Int -> Int
h n = f (g n)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z   |x >= y && x >= z = x
                |y >= x && y >= z = y
                |otherwise = z

absoluto :: Int -> Int
absoluto x  | x>=0 = x
            | otherwise = -x

absolutof :: Float -> Float
absolutof n     | n < 0 = -n
                |otherwise = n

digitoUnidades :: Int -> Int
digitoUnidades x = mod (absoluto x) 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod (absoluto x) 100) 10

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d)   | a<c && b<d = True
                        | otherwise = False

posPrimerPar :: Int -> Int -> Int -> Int
posPrimerPar a b c  | even a = 0
                    | even b = 1
                    | even c = 2
                    | otherwise = 4

bisiesto :: Int -> Bool
bisiesto x  | mod x 100 == 0 = mod x 400 == 0
            | otherwise = mod x 4 == 0

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (x,y,z) = absolutof (a-x) + absolutof (b-y) + absolutof (c-z)

comparar :: Int -> Int -> Int
comparar x y    | digitoUnidades x + digitoDecenas x < digitoUnidades y + digitoDecenas y = 1
                | digitoUnidades x + digitoDecenas x > digitoUnidades y + digitoDecenas y = -1
                | otherwise = 0

fibonacci :: Int -> Int
fibonacci n | n == 1 = 1
            | n == 0 = 0
            | otherwise = fibonacci (n-2) + fibonacci (n-1)

parteEntera :: Float -> Float
parteEntera n = int2Float (floor n)

esDivisible :: Int -> Int -> Bool
esDivisible a b = floor(int2Float a/ int2Float b)*b == a

sumaImpares :: Int -> Int
sumaImpares n   | n == 1 = 1
                | otherwise = sumaImpares (n-1) + 2*n-1

medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n == 2 = 2
            | otherwise = medioFact(n-2)*n

todosDigitosIguales :: Int -> Bool
todosDigitosIguales n   | n < 10 = True
                        | otherwise = (digitoUnidades n == digitoUnidades ( sacarUnidades n )) && todosDigitosIguales ( sacarUnidades n )

sacarUnidades :: Int -> Int
sacarUnidades n = div n 10

sumaDigitos :: Int -> Int
sumaDigitos n   | n==0 = 0
                | otherwise = mod n 10 + sumaDigitos(div n 10)

cantDigitos :: Int -> Int
cantDigitos n   | n == 0 = 0
                | otherwise = 1 + cantDigitos(div n 10)

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + length xs

ultimo :: [t] -> t
ultimo [a] = a
ultimo (_:xs) = ultimo xs

principio :: [t] -> t
principio [a] = a
principio (x:_) = x

reverso :: [t] -> [t]
reverso [a] = [a]
reverso [] = []
reverso (x:xs) = reverso xs ++[x]

pertenece :: Eq t => t -> [t] -> Bool
pertenece a [] = False
pertenece a (x:xs)  | a == x = True
                    | otherwise = pertenece a xs

todosiguales :: Eq t => [t] -> Bool
todosiguales [] = True
todosiguales [a] = True
todosiguales (x:xs) = x == head xs && todosiguales xs

todosdistintos :: Eq t => [t] -> Bool
todosdistintos [] = True
todosdistintos [a] = True
todosdistintos (x:xs) = not (pertenece x xs) && todosdistintos xs

hayrepetidos :: Eq t => [t] -> Bool
hayrepetidos [] = False
hayrepetidos [a] = False
hayrepetidos (x:xs) = pertenece x xs || hayrepetidos xs

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

sumarinternos :: [Int] -> Int
sumarinternos [] = 0
sumarinternos [a] = a
sumarinternos (x:xs) = x + (sumarinternos xs)

igualcandidatos :: (String, String) -> Bool
igualcandidatos a = (fst a) == (snd a)

estaotralista :: String -> [(String, String)] -> Bool
estaotralista a [] = False
estaotralista a [b] = a == (fst b) || a == (snd b)
estaotralista a (x:xs) = a == (fst x) || a == (snd x) || estaotralista a xs

cantidadvotos :: String -> [(String, String)] -> [Int] -> Int
cantidadvotos s (x1:xs1) (x2:xs2)   | s == (snd x1) = x2
                                    | otherwise = cantidadvotos s xs1 xs2 

esMenor :: Int -> [Int] -> Bool
esMenor a [b] = a<=b
esMenor a (x:xs) = a<=b || esMenor a xs

-- Ejercicio 1
porcentajeDeVotosAfirmativos :: [(String, String)] -> [Int] -> Int  -> Float
porcentajeDeVotosAfirmativos [] [] a = 0.0
porcentajeDeVotosAfirmativos s n a = (division (sumarinternos n) a)*100



-- Ejercicio 2
formulasInvalidas :: [(String, String)] -> Bool
formulasInvalidas [] = False
formulasInvalidas [a] = igualcandidatos a
formulasInvalidas (x:xs) = igualcandidatos x || estaotralista (fst x) xs || estaotralista (snd x) xs


-- Ejercicio 3
porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeVotos s c b = division (cantidadvotos s c b) (sumarinternos b)



-- Ejercicio 4 ESTOY EDITANDO MUAJAJAJAJ
menosVotado :: [(String, String)] -> [Int] -> String
menosVotado [a] [b] = fst a
menosVotado (x:xs) (y:ys)   | esMenor y ys = fst x
                            | otherwise = menosVotado xs ys

