-- eccetto alcuni problemi teorici
-- non ci sono esercizi particolari nel capitolo 2
last' :: [a] -> a
last' l = l !! (length l - 1)

last'' :: [a] -> a
last'' l = head (reverse l) 
