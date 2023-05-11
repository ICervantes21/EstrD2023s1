import PriorityQueue

pq1 :: PriorityQueue Int
pq1 = insertPQ 3 (insertPQ 5 (insertPQ 2 (insertPQ 1 emptyPQ)))

heapSort :: Ord a => [a] -> [a]
--Propósito: dada una lista la ordena de menor a mayor
heapSort xs = ordenar (newPQ xs)

newPQ :: Ord a => [a] -> PriorityQueue a
newPQ [] = emptyPQ
newPQ (x:xs) = insertPQ x (newPQ xs)

ordenar :: Ord a => PriorityQueue a -> [a]
ordenar pq = if isEmptyPQ pq
    then []
    else findMinPQ pq : ordenar (deleteMinPQ pq)

