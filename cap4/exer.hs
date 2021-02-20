l = [1..10]

-- 1)
halve :: [a] -> ([a], [a])
halve l = (take half l, drop half l)
  where half = length l `div` 2

-- solution proposed by hlint

{-
  def. of sliptAt

  splitAt :: Int -> [a] -> ([a], [a])
  splitAt n l = (take n l, drop n l)
-}

halve':: [a] -> ([a], [a])
halve' l = splitAt half l
  where half = length l `div` 2

-- 2)
safetail :: [a] -> [a]
safetail l = if null l then [] else tail l

safetail' :: [a] -> [a]
safetail' l | null l = []
            | otherwise = tail l

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' l = tail l

-- 3)
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True = True
True ||| False = True
True ||| True = True

(||||) :: Bool -> Bool -> Bool
False |||| False = False
_ |||| _ = True

(|||||) :: Bool -> Bool -> Bool
True ||||| _ = True
False |||||  b = b

(||||||) :: Bool -> Bool -> Bool
a |||||| b | a == b = a
             | otherwise = True

-- 4)
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then if b == True then True else  False else False
-- hlint suggest: that function is redundant

-- 5)
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else False

-- 6)
mult_lambda :: Int -> Int -> Int -> Int
mult_lambda = \x -> (\y -> (\z -> x * y * z))
