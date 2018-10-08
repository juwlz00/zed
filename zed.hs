
type Space = Int
type Rows = [Space]
type Cols = [Space]

--functions

isUnique :: [Space] -> Bool
isUnique [] = True;
isUnique (h:t) = if (h `elem` t) then False else isUnique t 