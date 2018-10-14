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

north :: Row -> Row
north row = [0] ++ row ++ [0];

weastHelper :: Int -> Int -> Row -> Row
weastHelper c1 c2 r = c1: (reverse(c2: reverse r))

weast :: [Int] -> [Int] -> [Row] -> [Row]
weast [] [] [] = [];
weast (c1:c1t) (c2:c2t) (r:rt) =  (weastHelper c1 c2 r):(weast c1t c2t rt) 

--gridArray :: Zed -> Clue -> [[Int]]
--gridArray (z:zt) (c:ct) = north z;

toString :: [Int] -> [String]
toString i = id (map show i);

--zedToString :: Zed -> [String]
--zedToString zed = foldr (\ x acc (map show) zed

toGrid :: [Int] -> [Char]
toGrid z = intercalate " " (toString z)

toGrid2 :: [[Int]] -> String
toGrid2 = lnBreak " " . (map (map show))

lnBreak :: String -> [[String]] -> String
lnBreak str = intercalate "\n" . map (intercalate str)


--([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])

gameSize :: Clue -> Int
gameSize clue = length clue

--Search for 
--searchN :: [[Int]] -> Zed -> [Int] 
--searchN clue zed = elemIndices 1 (head clue)

ind c = elemIndices 1 c

--

