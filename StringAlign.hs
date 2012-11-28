import Data.Char

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String, String)

----- 2a -----
similarityScore :: String -> String -> Int
{-similarityScore [] [] = 0
similarityScore (x:xs) [] = similarityScore xs [] + matchScore x '-'
similarityScore [] (y:ys) = similarityScore [] ys + matchScore '-' y
similarityScore (x:xs) (y:ys) = maximum [(similarityScore xs ys + matchScore x y), (similarityScore xs (y:ys) + matchScore x '-'),(similarityScore (x:xs) ys + matchScore '-' y)]-}
similarityScore xs ys = similarity (length xs) (length ys)
	where
		similarity i j = simTable!!i!!j
		simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
	
		simEntry :: Int -> Int -> Int
		simEntry 0 0 = 0
		simEntry i j
			|j == 0 = simEntry (i-1) 0 + matchScore x '-'
			|i == 0 = simEntry 0 (j-1) + matchScore '-' y
			|otherwise = maximum [(simEntry (i-1) (j-1) + matchScore x y), (simEntry (i-1) j + matchScore x '-'),(simEntry i (j-1) + matchScore '-' y)]
			where
				x = xs!!(i-1)
				y = ys!!(j-1)

matchScore :: Char -> Char -> Int
matchScore _ '-' = scoreSpace
matchScore '-' _ = scoreSpace
matchScore x y
			|x == y = scoreMatch
			|otherwise = scoreMismatch

----- 2b -----
-- Lägger till h1 och h2 som första element i respektive lista i alla tupler i listan aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

----- 2c -----
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

----- 2d -----
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [("","")]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = concat (maximaBy (optMatchScore.head) [(attachHeads x y (optAlignments xs ys)), (attachHeads x '-' (optAlignments xs (y:ys))), (attachHeads '-' y (optAlignments (x:xs) ys))])

optMatchScore :: AlignmentType -> Int
optMatchScore (string1, string2) = optScorer string1 string2

optScorer :: String -> String -> Int
optScorer [] _ = 0
optScorer (x:xs) (y:ys) = (matchScore x y) + (optScorer xs ys)

----- 2e -----
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
							putStrLn ("There are " ++ show (length aligns) ++ " optimal alignments")
							printAligns aligns
							where 	aligns = (optAlignments string1 string2)
									
											
printAligns :: [AlignmentType] -> IO()
printAligns [] = return ()
printAligns (x:xs) = do
					putStrLn ("\n" ++ (spaceString(fst x)) ++ "\n" ++ (spaceString(snd x)))
					printAligns xs

spaceString :: String -> String
spaceString s = concat [[c] ++ " " | c <- s]


----- 3 -----
table = [[ mcsEntry i j | j<-[0..9]] | i<-[0..9] ]
mcsEntry :: Int -> Int -> String
mcsEntry i j = show i ++ show j
							
							