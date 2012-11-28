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
		simTable = [[ simEntry i j | j <- [0..]] | i <- [0..] ]
	
		simEntry :: Int -> Int -> Int
		simEntry 0 0 = 0
		simEntry i j
			|j == 0 = simEntry (i-1) j + matchScore x '-'
			|i == 0 = simEntry i (j-1) + matchScore '-' y
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
-- L�gger till h1 och h2 som f�rsta element i respektive lista i alla tupler i listan aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

----- 2c -----
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

----- 2d -----
{-optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [("","")]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = concat (maximaBy (optMatchScore.head) [(attachHeads x y (optAlignments xs ys)), (attachHeads x '-' (optAlignments xs (y:ys))), (attachHeads '-' y (optAlignments (x:xs) ys))])

optMatchScore :: AlignmentType -> Int
optMatchScore (string1, string2) = optScorer string1 string2

optScorer :: String -> String -> Int
optScorer [] _ = 0
optScorer (x:xs) (y:ys) = (matchScore x y) + (optScorer xs ys)
-}

optAlignmentsOpt :: String -> String -> [AlignmentType]
optAlignmentsOpt xs ys = optAlign (length xs) (length ys)
	where
		optAlign i j = snd (optTable!!i!!j)
		optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
		
		optEntry :: Int -> Int -> (Int, [AlignmentType])
		optEntry 0 0 = (0, [("","")])
		optEntry i j
			|j == 0 = createOptEntry x '-' (optEntry (i-1) j)
			|i == 0 = createOptEntry '-' y (optEntry i (j-1))
			|otherwise = concatOptEntryList (maximaBy (fst.head) [createOptEntry x y (optEntry (i-1) (j-1)), createOptEntry x '-' (optEntry (i-1) j), createOptEntry '-' y (optEntry i (j-1))])
			where
				x = xs!!(i-1)
				y = ys!!(j-1)

createOptEntry :: Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
createOptEntry x y prevEntry = ((fst prevEntry)+ matchScore x y, attachTails x y (snd prevEntry))

concatOptEntryList :: [(Int, [AlignmentType])] -> (Int, [AlignmentType])
concatOptEntryList list = (head(fst unz),concat(snd unz))
	where unz = unzip list

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails t1 t2 aList = [(xs++[t1],ys++[t2]) | (xs,ys) <- aList]



----- 2e -----
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
							putStrLn ("There are " ++ show (length aligns) ++ " optimal alignments")
							printAligns aligns
							where 	aligns = (optAlignmentsOpt string1 string2)
									
											
printAligns :: [AlignmentType] -> IO()
printAligns [] = return ()
printAligns (x:xs) = do
					putStrLn ("\n" ++ (spaceString(fst x)) ++ "\n" ++ (spaceString(snd x)))
					printAligns xs

spaceString :: String -> String
spaceString s = concat [[c] ++ " " | c <- s]


----- 3 -----

							
							