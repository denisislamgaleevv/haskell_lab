
-- задание 1:
-- а) в строке есть цифры
-- any (flip elem ['0'..'9']) str
-- any (elem ['0'..'9'] "123456a")
-- any (flip elem ['0'..'9']) "aaaaaaaaa"
-- flip - (a -> b -> c) -> b -> a -> c 
 
import Data.List
import Debug.Trace
import Data.Char(isLower)
-- б) все символы строки - прописные буквы
-- all isLower str
-- all isLower  "Aaaa"
-- all isLower   "aaaa" 
anyNum str = any (\x -> elem x "1234567890") str

-- задание 2:
-- а) получить список, в котором каждый элемент складывается со своим индексом
 
addIndex2 xs = zipWith (+) xs [0..]

-- б) удалить из конца полученного списка все четные элементы, следующие за последним нечетным
deleteLastEven xs = reverse (dropWhile even (reverse xs))











-- задание 3:

-- inverse [(2,2),(4,4),(1,2),(3,1),(3,4)]
-- uniset [(2,2),(4,4),(1,2),(3,1),(3,4)]
-- unisetPQ  [(2,2),(4,4),(1,2),(3,1),(3,4)]  [(2,2),(4,4),(1,2),(3,1),(3,4)] 
-- composition [(2,2),(4,4),(1,2),(3,1),(3,4)]  [(2,2),(4,4),(1,2),(3,1),(3,4)] 
-- pr1 [(2,2),(4,4),(1,2),(3,1),(3,4)] 
-- pr2 [(2,2),(4,4),(1,2),(3,1),(3,4)] 
-- productDescart [(2,2),(4,4),(1,2),(3,1),(3,4)] [(2,2),(4,4),(1,2),(3,1),(3,4)] 

-- а) обратное отображение
inverse xs = map (\(x, y) -> (y, x)) xs

-- б) композиция
--нельзя использовать рекурсию
 
-- zip [1,2,3,4,5] [9,8]
-- [(1,9),(2,8)]

-- zipWith (+) [1,2,3] [3,2,1]
-- [4,4,4]

-- unzip [(1,2),(2,3),(3,4)]
-- ([1,2,3],[2,3,4])

 
s1 :: [(Integer, Integer)] -> [(Integer, Integer)]
s1 a = zip (snd (unzip a)) (fst (unzip a))

s2 :: [(Integer, Integer)] -> [(Integer, Integer)]
s2 arg = nub [(a,c) | (a,b) <- arg, (b,c) <- arg]

s3 :: [(Integer, Integer)] -> [(Integer, Integer)]
s3 arg = nub [(a,c) | (a,b) <- s1 arg, (b,c) <- arg]

s4 :: [(Integer, Integer)] -> [(Integer, Integer)]
s4 arg = nub [(a,b) | a <- snd (unzip (s3 arg)),
                             b <- fst (unzip (s2 arg))]                             
 
-- [(2, 2), (4, 4), (1,2), (3, 1), (3, 4)]















-- задание 4:
-- solution  [(1,2),(3,3),(5,4), (1,8), (1,8)]
p1 :: Integer -> Integer -> Bool
p1 x y = even (x+y)

p2 :: Integer -> Integer -> Bool
p2 x y = x > y

p3 :: Integer -> Integer -> Bool
p3 x y = x `mod` 4 == y `mod` 4

p4 :: Integer -> Integer -> Bool
p4 x y = x+2*y < 8

p5 :: Integer -> Integer -> Bool
p5 x y = not (even (max x y))

getArr nums = [[p1 a b | (a,b) <- nums],
               [p2 a b | (a,b) <- nums],
               [p3 a b | (a,b) <- nums],
               [p4 a b | (a,b) <- nums],
               [p5 a b | (a,b) <- nums]]

solution [] = []
solution list = map (or) (getArr list)

-- [True,False,True,True,True]










-- задание 5:
-- function [[1,2,3,4,5,6,7,8,9], [5,6,7,8,9],  [1,2,3,4], [1,2,3,4,5,6,7,8,9]]  2 4 
-- function [[1,2,3,4,5,6,7,8,9], [5,6,7,8,9],  [1,2,3,4], [1,2,3,4,5,6,7,8,9]]  4 4 
-- function [[1,2,3,4,5,6,7,8,9], [5,6,7,8,9],  [1,2,3,4], [1,2,3,4,5,6,7,8,9]]  5 4 
function xs i j = map ((take (j - i + 1)).(drop i)) (filter (\xs1 -> length xs1 > j) xs)
 
-- задание 6:
colors  =  [(x, y, z) | x <- colors, y <- colors, z <- colors, x /= y,  y /= z] where colors = ["Black",  "White",  "Red", "Blue", "Yellow", "Green", "Orange", "Purple", "Grey"]
tcolors = 7*8*9 




 
 