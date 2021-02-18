sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lower ++ [x] ++ qsort higher
  where lower = [a | a <- xs, a <= x]
        higher = [b | b <- xs, b > x]

msort:: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort (take half l)) (msort (drop half l))
          where half = length l `div` 2
                merge [] ys = ys
                merge xs [] = xs
                merge (x:xs) (y:ys) = if x < y
                  then x : merge xs (y : ys)
                  else y : merge (x : xs) ys
