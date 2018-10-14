import Data.List

type Space = Int
type Row = [Space]
type Zed = [Row]
type Clue = [[Int]]

--functions	

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


--([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])

gameSize :: Clue -> Int
gameSize clue = length clue

--Search for 
--searchN :: [[Int]] -> Zed -> [Int] 
--searchN clue zed = elemIndices 1 (head clue)

ind c = elemIndices 1 c

--

