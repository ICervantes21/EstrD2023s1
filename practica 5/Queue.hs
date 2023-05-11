module Queue

    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

where

data Queue a = Q [a] deriving Show

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (agregarAlFinal xs x)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (x:xs) y = x: agregarAlFinal xs y

firstQ :: Queue a -> a
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (tail xs)