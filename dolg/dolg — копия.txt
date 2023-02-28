import Data.List
import Debug.Trace
--ЛАБА 2

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

fromRome:: Char -> Integer
fromRome x
                                             | x == 'I'  = 1  
                                             | x == 'V'  = 5   
                                             | x == 'X'  = 10  
                                             | x == 'L'  = 50  
                                             | x == 'C'  = 100 
                                             | x == 'D'  = 500 
                                             | x == 'M'  = 1000
                                             | otherwise = 0


 
translateRome []= 0
translateRome (h:[]) =  (fromRome h) 
translateRome (h:num) | fromRome (head(num))>=(fromRome h) =  -(fromRome h)+ (translateRome num) 
                      | True =  (fromRome h) + (translateRome num) 
                      
                                            

calculate text = (snd (findoper text 0)) (translateSS (getSS text) a) (translateRome b)
                     where a = takeWhile (/= '(') (fst (split text))
                           b = snd (split text) 

-- calculate "A10(16)-DCCLIII"      
-- xx where xx = [trace (show x) fromRome x | x <- ['I', 'V', 'X', 'L', 'C', 'D', 'M']]
--Функция trace выводит строку, переданную первым аргументом, и возвращает то, что было передано вторым аргументом 

--ЛАБА 3

--задание 1 
anyNum str = any (\x ->elem x "1234567890") str


-- задание 3
-- nub [1,2,3,1,2,3,1,1,2,3] = [1,2,3]
-- zip [1,2,3] [9,8,7] = [(1,9),(2,8),(3,7)]
-- unzip [(1,2),(2,3),(3,4)]  = ([1,2,3],[2,3,4])

s1 :: [(Integer, Integer)] -> [(Integer, Integer)]
s1 a = zip (snd (unzip a)) (fst (unzip a))

s2 :: [(Integer, Integer)] -> [(Integer, Integer)]
s2 arg = nub [(a,c) | (a,b) <- arg, (b,c) <- arg]

s3 :: [(Integer, Integer)] -> [(Integer, Integer)]
s3 arg = nub [(a,c) | (a,b) <- s1 arg, (b,c) <- arg]

s4 :: [(Integer, Integer)] -> [(Integer, Integer)]
s4 arg = nub [(a,b) | a <- snd (unzip (s3 arg)),
                             b <- fst (unzip (s2 arg))]                             
 
z3 = [(2, 2), (4, 4), (1,2), (3, 1), (3, 4)]

 

-- 3_4 

 
 
get_list = ([(x,y)|x<-[1..8],y<-[1..8]])

p1 (x,y) = even (x+y) 
p2 (x,y) = x >y
p3 (x,y) = (mod x 4) == (mod y 4)
p4 (x,y) = (x+2*y)<8
p5 (x,y) = odd (max x y)

ps = [p1,p2,p3,p4,p5]

myEq (x,_) (y,_) = x == y
myGroup list = groupBy (myEq) (sort list)


f2 p list = map (map p) (myGroup list)

f3 p list = foldr (||) False (map (and) (f2 p list))

f4 plist list = [f3 p list|p<-plist] 
-- f4 ps [(2,3), (1,3),  (1,8), (2,5)]