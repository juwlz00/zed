import Data.List

type Space = Int
type Rows = [Space]
type Zed = [Rows]
type Clue = [[Int]]

--functions	

isUnique :: [Space] -> Bool
isUnique [] = True;
isUnique (h:t) = if (h `elem` t) then False else isUnique t 

--([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])

gameSize :: Clue -> Int
gameSize clue = length clue

--searchN :: [[Int]] -> Zed -> [Int] 
--searchN clue zed = elemIndices 1 (head clue)

ind c = elemIndices 1 c