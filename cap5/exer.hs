import Data.Char

-- caesar cipher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | isUpper c = toUpper(int2let((let2int (toLower c) + n) `mod` 26))
          | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]
-- omg haskell is so elegant

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
  
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
  where n = length xs - 1

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = 100 * (a / b)
  where a = fromIntegral n :: Float
        b = fromIntegral m :: Float

freqs :: String -> [Float]
freqs s = [percent (count c s) n | c <- ['a' .. 'z']]
  where n = lowers s

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n s = drop n s ++ take n s

crack :: String -> String
crack s = encode (-factor) s
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs s

-- 1)
square = [x^2 | x <- [1..100]]

square' :: Int -> [Int]
square' n = [x^2 | x <- [1..n]]

-- 2)
replicate' :: Int -> a -> [a]
replicate' n val = [val | _ <- [1..n]]

-- 3)
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y ,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 4)
factors :: Int -> [Int]
factors n = [factor | factor <- [1..n], n `mod` factor == 0]

perfect :: Int -> Bool
perfect n = (sum.factors) n == 2*n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

-- 5)
l  = [(x, y) | x <- [1,2,3], y <- [4,5,6]] 
l' = concat [[(x, y) | x <- [1,2,3]] | y <- [4,5,6]]
l'' = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6)
find' :: Eq a => a -> [(a, b)] -> [b]
find' k t = [v | (k', v) <- t, k' == k]

position' :: Eq a => a -> [a] -> [Int]
position' k [] = []
position' k t = find' k (zip t [0..])

-- 7)
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x, y) <- zip xs ys]


