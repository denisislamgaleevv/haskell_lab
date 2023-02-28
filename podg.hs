dropc :: [a]->Int->[a]
dropc xs 0 = xs
dropc [] _ = []
dropc (x:xs) n = dropc xs (n-1)


take1:: Int ->[a]->[a]
take1 0 xs = []
take1 _ [] = error "index too large"
take1 n (x:xs) = x: take1 (n-1) xs

--myzip [1..5] ['a'..'z'] → [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')].  

myzip :: [a]->[b]->[(a,b)]
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys 
myzip _ _ = [] 

factorial:: Integer -> Integer
factorial x = factorial_a x 1

factorial_a :: Integer -> Integer -> Integer
factorial_a 0 acc = acc
factorial_a n acc = factorial_a (n-1) (acc*n)

foo1:: Int -> Int -> String -> String
foo1 a b xs  |  a <= 0 = take b xs 
             | otherwise = take b (drop a xs)
 