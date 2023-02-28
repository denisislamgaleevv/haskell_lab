-- Лабораторная работа №4 (Haskell)
-- Вариант 1

import System.IO
import Char
import Data.Ratio
 
import Data.List


-- задание 1:
five = [truncate (n * (3 * n - 1) / 2) | n <- [1..]] 
-- take 10 five
colChLessThenThus = length (filter even (takeWhile (<1000) [truncate (n * (3 * n - 1) / 2) | n <- [1..]]))


-- задание 2:
input_output :: IO ()
input_output = do
                text <- readFile "input.txt"
                let strs = lines text
                writeFile "output.txt" (connection (sort strs))
                  where sort [] = []
                        sort (x:xs) = (sort [y | y <- xs, length y < length x])
                                           ++ [x] ++
                                      (sort [y | y <- xs, length y >= length x])

                        connection [] = []
                        connection (y:ys) = y ++ "\n" ++ (connection ys)
 
 

--57
main57 = putStrLn (show ans)
ans = sum [1 | b <- take 1000 (contFracSeq 0 1), b]

contFracSeq :: Integer -> Integer -> [Bool]
contFracSeq n d = let
		numer = d
		denom = d * 2 + n
	in (length (show (numer + denom)) > length (show denom)) : (contFracSeq numer denom)




--65
e = 2 : concat [ [1, 2*i, 1] | i <- [1..] ]
 
fraction [x] = x%1
fraction (x:xs) = x%1 + 1/(fraction xs)

main65 = sum $ map digitToInt $ show $ numerator $ fraction $ take 100 e









-- задание 3: (57)
sqrt2 :: Integer -> (Integer, Integer)
sqrt2 n = f (3, 2) (7, 5) n
           where f (a, b) (x, y) n | n == 1 = (a, b)
                                   | n == 2 = (x, y)
                                   | otherwise = f (x, y) (2 * x + a, 2 * y + b) (n - 1)
check (a, b) = length (show a) > length (show b)
cnt n | n == 8 = 1
      | otherwise = if (check (sqrt2 n)) then (1 + cnt (n - 1))
                                       else (cnt (n - 1))
--cnt 100


 
-- задание 4: (65)
-- числитель
foo n = reverse (f [11,8,3,2] n 1) !! (n - 1)
         where f xs k acc | (length xs) >= k * 3 = xs
                          | otherwise = f (c:b:a:xs) k (acc + 1)
                              where x = head xs
                                    y = head (tail xs)
                                    a = x + y
                                    b = 2 * (acc + 1) * a + x
                                    c = a + b
-- знаменатель
goo n = reverse (g [4,3,1,1] n 1) !! (n - 1)
         where g xs k acc | (length xs) >= k * 3 = xs
                          | otherwise = g (c:b:a:xs) k (acc + 1)
                              where x = head xs
                                    y = head (tail xs)
                                    a = x + y
                                    b = 2 * (acc + 1) * a + x
                                    c = a + b

-- сумма цифр числа
numsum x | div x 10 == 0 = x
         | otherwise = (mod x 10) + numsum (div x 10)

 

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


foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
