import Data.List

type Space = Int
type Row = [Space]
type Zed = [Row]
type Clue = [[Int]]

--Main function
{--
main =
	do
		putStrLn "Please enter number of clues:"
		ansNum <- getLineFixed
		putStrLn "Now enter the corresponding clue matrix:"
		ansClues <- getLineFixed
		return ansClues
		--createZed ansNum ansNum	  
--}
--validation functions	
isUnique :: Row -> Bool
isUnique [] = True;
isUnique (h:t) = if (h `elem` t) then False else isUnique t 

--Create starting game functions
createSpace = 0;

createRow :: Int -> Row
createRow 0 = [];
createRow i = createSpace : createRow (i-1);

createZed :: Int -> Int -> Zed 
createZed 0 j = [];
createZed i j = (createRow j) : (createZed (i-1) j);

--Grid building

toString :: [Int] -> [String]
toString i = map show i;

--zedToString :: Zed -> [String]
--zedToString zed = foldr (\ x acc (map show) zed

toGrid :: [Int] -> [Char]
toGrid z = intercalate " " (toString z)


--([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])

gameSize :: Clue -> Int
gameSize clue = length clue

--Search for 
--searchN :: [[Int]] -> Zed -> [Int] 
--searchN clue zed = elemIndices 1 (head clue)

ind c = elemIndices 1 c

--

