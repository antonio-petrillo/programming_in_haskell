import Data.Char
  
square = [x^2 | x <- [1..10]]

-- it's a cartesian product between two list
tuples = [(x, y) | x <- [1, 2, 3], y <- [4, 5]]
tuples' = [(x, y) | x <- [4, 5], y <- [1, 2, 3]]

tuples''' = [(x, y) | x <- [1..3], y <- [x..3]]

-- definition of concat
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find' :: Eq a => a -> [(a, b)] -> [b]
find' k t = [v | (k', v) <- t, k' == k]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

pairs :: [a] -> [(a, a)]
pairs l = zip l (tail l)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' l = foldl (&&) True l

and''' :: [Bool] -> Bool
and''' = foldl (&&) True

and'''' :: [Bool] -> Bool
and'''' l = foldr (&&) True l

and''''' :: [Bool] -> Bool
and''''' = foldr (&&) True 

sorted :: Ord a => [a] -> Bool
sorted l = and[x <= y | (x, y) <- pairs l]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip' xs [0..n], x == x']
  where n = length xs - 1

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

