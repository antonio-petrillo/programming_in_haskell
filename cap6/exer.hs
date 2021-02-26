insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x $ isort xs

isort' :: Ord a => [a] -> [a]
isort' l = foldr insert [] l

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

even' :: Int -> Bool
even' 0 = True
even' n = odd' $ n-1

odd' :: Int -> Bool
odd' 0 = False
odd' n = even $ n-1

even'' :: Int -> Bool
even'' 0 = True
even'' n = even'' $ n-1

even''' :: Int -> Bool
even''' n = n `mod` 2 == 0

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs
