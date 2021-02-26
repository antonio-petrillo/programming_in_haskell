-- 1) pow definition
-- Attention  0 to the 0 return 1 but it should be 'division by zero'
(@@) :: Int -> Int -> Int
x @@ 0 = 1
x @@ y = x * (x @@ (y-1))

{-

example 2 @@ 3

2 * ( 2 @@ (3 - 1))
applying -
2 * ( 2 @@ 2)
2 * ( 2 * ( 2 @@ (2-1)))
applying -
2 * ( 2 * ( 2 @@ 1) )
2 * ( 2 * ( 2 * ( 2 @@ (1-1)) ) )
applying -
2 * ( 2 * ( 2 * ( 2 * 1)) ) )
applying *
2 * ( 2 * 2) )
applying *
2 * 4 
applying *
8

result 8
-}

-- 2)
-- to boring to write on a pc

-- 3)
and' :: [Bool] -> Bool
and' l = foldr (&&) True l

and'' :: [Bool] -> Bool
and'' [] = True
and'' (x:xs) = x && (and'' xs)

concat' :: [[a]] -> [a]
concat' l = foldr (++) [] l

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

concat''' :: [[a]] -> [a]
concat''' xss = [x | xs <- xss, x <- xs]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

(!!!) :: [a] -> Int -> a
x !!! n | n == 0 = head x
        | otherwise = (tail x) !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool -- member in prolog
elem' _ [] = False
elem' x (y:ys) = if x == y then True else elem' x ys
  
elem'' :: Eq a => a -> [a] -> Bool
elem'' _ [] = False
elem'' x (y:ys) = x == y || elem'' x ys

elem''' :: Eq a => a -> [a] -> Bool
elem''' x xs = foldr (\a b -> a == x || b) False xs

elem'''' :: Eq a => a -> [a] -> Bool
elem'''' x xs = foldl (\a b -> a || b == x) False xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
                    
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ take half xs) (msort $ drop half xs)
  where half = length xs `div` 2

halve :: [a] -> ([a], [a])
halve l = splitAt ((length l) `div` 2) l

msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' [x] = [x]
msort' xs = merge (msort' left) (msort' right)
  where (left, right) = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' l = foldr (+) 0 l

sum''' :: Num a => [a] -> a
sum'''  = foldr (+) 0

sum'''' :: Num a => [a] -> a
sum'''' l = foldl (+) 0 l

sum''''' :: Num a => [a] -> a
sum''''' = foldl (+) 0 

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 (x:_) = [x]
take' n (_:xs) = take' (n-1) xs

last' :: [a] -> a
last' l = l !! (length l -1)

last'' :: [a] -> a
last'' = head . reverse

last''' :: [a] -> a
last''' (x:[]) = x
last''' (_:xs) = last''' xs

