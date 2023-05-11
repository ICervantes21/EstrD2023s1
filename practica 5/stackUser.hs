import Stack

s1 :: Stack Int
s1 = (push 5 (push 4 (push 3 (push 2 (push 1 (emptyS))))))

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)