import Data.Char
-- String transmitter

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2:int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map(make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits:chop8(drop 8 bits)

decode :: [Bit] -> String
decode = map(chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode


-- .1 [f x | x <- xs , p x] con map e filter
exer1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exer1 f p l = map f (filter p l)

-- .2
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x acc -> p x && acc) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldl (\acc x -> acc || p x ) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x acc -> if p x then x:acc else []) []

-- con le fold e' piu' complicata
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

append :: a -> [a] -> [a]
append x [] = [x]
append x (y:[]) = y:[x]
append x (y:ys) = y:append x ys
 
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldl (\acc x -> if p x then append x acc else acc) []

type Dec = Int

dec2int' :: [Dec] -> Int
dec2int' = foldl (\acc x -> acc*10 + x) 0

-- .5
compose :: [a -> a] -> (a -> a)
compose  = foldr (.) id

{-
sumsqreven = compose [sum, map (^2), filter even]
non puo' funzionare dato che il tipo di map e filter e' [a] -> [a]
mentre sum e' [a] -> a
quindi una volta inizializzata sumsqreven (quando parte dal fondo) 
assume il tipo [a] -> [a], e quando arrivo a sum non e' compatibile
-}
-- versione corretta
sumsqreven :: Integral a => [a] -> a  
sumsqreven l = sum (map (^2) (filter even l))

-- .6
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \a b -> f(a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b

-- .7 Da rivedere meglio la unfold
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod`2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f.head) tail

iterate' :: (a -> a)  -> a -> [a]
iterate' = unfold (const False) id

parity :: [Bit] -> Bit
parity xs = foldr (\x acc -> if x == 1 then acc+1 else acc ) 0 xs `mod` 2

parity' :: [Bit] -> Bit
parity' xs = sum xs `mod` 2

addParity :: [Bit] -> [Bit]
addParity xs = parityBit:xs
  where parityBit = parity xs

encodeParity :: String -> [Bit]
encodeParity = concat . map(addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits:chop9(drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity xs | head xs == (parity.tail) xs = tail xs
               | otherwise = error "Parity check failed!"

decodeParity :: [Bit] -> String
decodeParity = map (chr . bin2int . checkParity) . chop9

transmit' :: String -> String
transmit' = decodeParity . channel . encodeParity

faulty :: [Bit] -> [Bit]
faulty [] = []
faulty xs = tail xs 


transmit'' :: String -> String
transmit'' = decodeParity . faulty . encodeParity
