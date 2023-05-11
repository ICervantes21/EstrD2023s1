import Queue

q1 :: Queue Int
q1 = (enqueue 1 (enqueue 2 (enqueue 3 (enqueue 5 (emptyQ)))))

q2 :: Queue Int
q2 = (enqueue 9 (enqueue 8 (enqueue 7 (enqueue 6 (emptyQ)))))

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
    then 0
    else 1 + lengthQ (dequeue q)


queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
    then []
    else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = agregarVariosQ q1 (queueToList q2)

agregarVariosQ :: Queue a -> [a] -> Queue a
agregarVariosQ q [] = q
agregarVariosQ q (x:xs) = enqueue x (agregarVariosQ q xs)

newQ :: [a] -> Queue a
newQ [] = emptyQ
newQ (x:xs) = enqueue x (newQ xs)
