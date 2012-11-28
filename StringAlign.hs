import Data.Char

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore x [] = (length x) * scoreSpace
similarityScore [] y = (length y) * scoreSpace
similarityScore (x:xs) (y:ys) = maximum [(similarityScore xs ys + matchScore x y), (similarityScore xs (y:ys) + matchScore x '-'),(similarityScore (x:xs) ys + matchScore '-' y)]

matchScore :: Char -> Char -> Int
matchScore _ '-' = scoreSpace
matchScore '-' _ = scoreSpace
matchScore x y
			|x == y = scoreMatch
			|otherwise = scoreMismatch

-- Lägger till h1 och h2 som första element i respektive lista i alla tupler i listan aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]