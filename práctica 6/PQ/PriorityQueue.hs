module PriorityQueue

    (PriorityQueue, emptyPQ, isEmptyPQ
     , insertPQ, findMinPQ, deleteMinPQ) 
where

data PriorityQueue a = Q[a] deriving Show

emptyPQ :: PriorityQueue a
emptyPQ = Q[]

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (Q xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (Q xs) = Q (agregarAlFinal xs x)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (x:xs) y = x: agregarAlFinal xs y

findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (Q xs) = minimum xs



deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (Q xs) =  Q (delete (minimum xs) xs)

delete :: Ord a => a -> [a] -> [a]
delete _ [] = []
delete y (x:xs) = if y == x
    then xs
    else x : delete y xs