import Set

set1 :: Set Int
set1 = (addS 1 (addS 2 (addS 3 (addS 4 (emptyS)))))

set2 :: Set Int
set2 = (addS 4 (addS 6 (addS 7 (addS 8 (emptyS)))))


losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = if belongs x s
    then x : losQuePertenecen xs s
    else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []    
sinRepetidos xs = setToList (nuevoSet xs)


nuevoSet ::Eq a => [a] -> Set a
nuevoSet [] = emptyS
nuevoSet (x:xs) = addS x (nuevoSet xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) = unionS (unionS (nuevoSet (setToList s)) (unirTodos t1)) (unionS (nuevoSet (setToList s)) (unirTodos t2))

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol :: Tree (Set Int)
arbol = NodeT set1
                  (NodeT set1
                          (NodeT set1
                                   (NodeT set1 EmptyT EmptyT)
                                   (NodeT set1 EmptyT EmptyT))
                            EmptyT)
                  (NodeT set1
                          (NodeT set1 EmptyT EmptyT)
                          (NodeT set1 EmptyT (
                                           NodeT set1 EmptyT EmptyT))) 