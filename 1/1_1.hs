
one  =  7.03**(1-sqrt 4.92)/ 3*4.92- tan 7.03
two = (not(mod (maximum[1..5] - minimum[1..5]) 2 > 0 ) || ([1..5]!!2/= minimum[1..5]))
four a b c = a**(1-sqrt b) / c*b- tan a


five :: Char -> Char -> String -> String
five  p z [] = []
five  p z (h:t) | h == p = z : five  p z t
                     | otherwise = h : five  p z t



fibb :: Integer -> Integer
fibb n = f 1 1 n where f x y n | n == 1 = x
                              | n == 2 = y
                              | n == 3 = x + y
                              | otherwise = f y (x + y) (n - 1)

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

rez :: Integer -> Integer -> Integer
rez n m = if ((n + m) `mod` 2) == 1
		then (fact(n) - fibb(m))
	     else fibb((n + m) `div` 2) - fact((n + m) `div` 2)






-- задание 3:
findoper (x:xs) i | x == '+'  = (i, (+))
                  | x == '-'  = (i, (-))
                  | otherwise = findoper xs (i + 1)

split text = if (elem ')' (take k text)) then (take k text, drop (k + 1) text)
                                         else (drop (k + 1) text, take k text)
             where k = fst (findoper text 0)

getSS text = read (drop 1 (takeWhile (/= ')') (dropWhile (/= '(') text))) + 0

translateSS k num = f k (reverse num) 0 where f k   ""   _                     = 0
                                              f k (x:xs) i | elem x ['0'..'9'] = (read [x] + 0) * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'A'          = 10 * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'B'          = 11 * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'C'          = 12 * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'D'          = 13 * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'E'          = 14 * (k ^ i) + (f k xs (i + 1))
                                                           | x == 'F'          = 15 * (k ^ i) + (f k xs (i + 1))
                                                           | otherwise         = 0

translateRome num = f num 0 where f   ""   _             = 0
                                  f (x:xs) i | x == 'I'  = 1    + f xs (i + 1)
                                             | x == 'V'  = 5    + f xs (i + 1)
                                             | x == 'X'  = 10   + f xs (i + 1)
                                             | x == 'L'  = 50   + f xs (i + 1)
                                             | x == 'C'  = 100  + f xs (i + 1)
                                             | x == 'D'  = 500  + f xs (i + 1)
                                             | x == 'M'  = 1000 + f xs (i + 1)
                                             | otherwise = 0

calculate text = (snd (findoper text 0)) (translateSS (getSS text) a) (translateRome b)
                     where a = takeWhile (/= '(') (fst (split text))
                           b = snd (split text)