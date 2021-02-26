twice :: (a -> a) -> a -> a
twice f x = (f . f) x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x : xs) []

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p l = foldr (\x xs -> if p x then x : xs else xs) [] l

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p  = foldr (\x xs -> if p x then x : xs else xs) []

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p xs = [x | x <- xs, p x]

filter'''' :: (a -> Bool) -> [a] -> [a]
filter'''' p [] = []
filter'''' p (x:xs) | p x = x : filter'''' p xs
                    | otherwise = filter'''' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

{-

foldr:
f [] = v
f (x:xs) = x (op) f xs
  |
  |  # <- op
  V
[x0, x1, x2, x3, ..., xn] v = x0 # (x1 # (x2 # (x3 # (... # (xn # v)))))
-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldr (\x xs -> xs ++ [x]) []

{-

foldl:
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
 |
 | # <- op
 V
v [x0, x1, x2, x3, ..., xn] = (((((v # x0) # x1) # x2) # x3) # ... # xn)
-}

sum' = sum'' 0
  where sum'' v [] = v
        sum'' v (x:xs) = sum'' (v+x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs)  = foldl' f (f v x ) xs

