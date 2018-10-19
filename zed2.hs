module Zed where

import Data.List
import System.Random
import System.IO.Unsafe
import Control.Monad

type Column = [Int]
type Row = [Int]
type Zed = [Row]
type Clue = ([Int],[Int],[Int],[Int])

--Main Functions------------------------------------------------------------------------------------------------------------------------

-- Given clues, returns a solved zed board by iterating through all valid permutations
solveZed :: Clue -> Zed           
solveZed (n,s,e,w) = solveZed' (n,s,e,w) (getValidBoards (length n))
solveZed' clues (h:t)
 |checkSolution clues h = h
 |otherwise = solveZed' clues t
{-
>solveZed ([2,1],[1,2],[2,1],[1,2])
[[1,2],[2,1]]
>solveZed ([2,2,1],[1,2,2],[3,1,2],[2,1,3])
[[1,2,3],[3,1,2],[2,3,1]]
>solveZed ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0])
[[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-}

-- Given clues, returns a solved zed board by guessing random permutations
solveZedRandom :: Clue -> Zed
solveZedRandom (n,s,e,w) = randomGuesser (n,s,e,w) (length n) (randomZed (length n) [] 0) (permutations[1..(length n)]) (factorial (length n))
{-
solveZedRandom ([2,1],[1,2],[2,1],[1,2])
[[1,2],[2,1]]
-}
-- Given clues, returns a solved zed board displayed as a grid
solveZedGrid clues = displayGrid clues (solveZed clues)
{-
>solveZedGrid ([2,1],[1,2],[2,1],[1,2])
0 | 2 | 1 | 0
-------------
2 | 1 | 2 | 1
-------------
1 | 2 | 1 | 2
-------------
0 | 1 | 2 | 0
>solveZedGrid ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0])
0 | 0 | 1 | 2 | 0 | 0
---------------------
0 | 1 | 4 | 3 | 2 | 0
---------------------
0 | 2 | 3 | 1 | 4 | 0
---------------------
0 | 4 | 1 | 2 | 3 | 0
---------------------
0 | 3 | 2 | 4 | 1 | 2
---------------------
0 | 0 | 3 | 0 | 3 | 0
-}

--Solution Generation-------------------------------------------------------------------------------------------------------------------

-- Given clues and a zed board, returns true if the zed board is correct
checkSolution :: Clue -> Zed -> Bool
checkSolution clues zed = (checkValidNumbers zed) && (checkNotFools clues zed)

-- Randomly guesses permutations of zed boards until a solution is found
randomGuesser clues n try perms fac
 |checkSolution clues try = try
 |otherwise = randomGuesser clues n (randomZed n perms fac) perms fac

-- Generates a random zed board
randomZed n [] _ = [[]]
randomZed n perms fac = randomZed2 n n fac perms
randomZed2 n count fac perms
 |count>0 = perms !! unsafePerformIO (getStdRandom (randomR (0, fac-1)))  : randomZed2 n (count-1) fac perms
 |otherwise = []

-- Given n, returns the nth factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Given n, returns a list of all valid zed boards (eg boards that have no duplicate numbers in columns/rows)
getValidBoards :: Int -> [Zed] 
getValidBoards n = removeDuplicatesZeds (getAllBoards n (permutations [1..n]))

-- Given a list of zed boards, removes all boards that have invalid columns
removeDuplicatesZeds :: [Zed] -> [Zed]
removeDuplicatesZeds zeds = filter checkDuplicatesZed zeds

-- Given a zed board, returns True if the board has no invalid columns
checkDuplicatesZed :: Zed -> Bool
checkDuplicatesZed zed = and (map checkDuplicatesList (allColumns zed))

-- Given a zed board, returns all the North->South columns as lists
allColumns :: Zed -> [Column]
allColumns zed = allColumns' (length zed) zed
allColumns' 0 _ = []
allColumns' n zed = (column (n-1) zed) : allColumns' (n-1) zed
{-
>allColumns [[4,2,1,3],[1,3,2,4],[3,1,4,2],[2,4,3,1]]
[[3,4,2,1],[1,2,4,3],[2,3,1,4],[4,1,3,2]]
-}
-- Given a column number and a zed board, returns the column at that index as a list
column :: Int -> Zed -> Column
column _ [] = []
column n (h:t) = (h!!n) : (column n t)
{-
>column 1 [[4,2,1,3],[1,3,2,4],[3,1,4,2],[2,4,3,1]]
[2,3,1,4]
-}

-- Given a list, returns True if the list has no duplicates
checkDuplicatesList :: Row -> Bool
checkDuplicatesList [] = True
checkDuplicatesList (h:t)
    | (h `elem` t) = False
    | otherwise = checkDuplicatesList t

-- Given n and all permutations of size n, generates all possible permutations for a zed board of size n
-- https://stackoverflow.com/questions/42503017/haskell-choose-l-n-function
getAllBoards ::  Int -> [[Int]] -> [Zed]
getAllBoards n perms = (choose perms n) >>=  permutations 
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

--Solution Validation-------------------------------------------------------------------------------------------------------------------

-- Given a zed board, checks that each row & column contains the numbers 1-n
checkValidNumbers zed = (checkValidNumbers' (transposeZed zed)) && (checkValidNumbers' zed)

-- intersect that handles duplicates https://stackoverflow.com/questions/27332815/haskell-intersection-with-duplicates
intersectD xs ys = xs \\ (xs \\ ys)

checkValidNumbers' [] = True
checkValidNumbers' (h:t) = (h `intersectD` [1..(length(h))] == h) && checkValidNumbers' t

reverseList xs = foldl (\x y -> y:x) [] xs 

reverseLists [] = []
reverseLists (h:t) = (foldl (\x y -> y:x) [] h) : (reverseLists t)

-- Given a zed board, returns a transposed version of the board (a list of the columns)
--https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
transposeZed ([]:_) = []
transposeZed x = (map head x) : transposeZed (map tail x)
{-
>transposeZed [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
[[1,2,4,3],[4,3,1,2],[3,1,2,4],[2,4,3,1]]
-}

-- Given clues and a zed board, returns True if the merchants are not fools (they only go to outposts if they are better than previous outposts)
checkNotFools :: Clue -> Zed -> Bool
checkNotFools clues [[]] = False
checkNotFools clues zed = checkSouthNorth clues zed && checkNorthSouth clues zed && checkEastWest clues zed && checkWestEast clues zed
{-
>checkNotFools ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
True
>checkNotFools ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[3,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
False
-}

-- Given clues and a zed board, maniplates input so that the South->North columns can be read left ->right and returns True if valid
checkSouthNorth :: Clue -> Zed -> Bool
checkSouthNorth (n,e,s,w) zed = checkLeftToRight (reverseList s) (reverseLists (transposeZed zed))

-- Given clues and a zed board, maniplates input so that the North->South columns can be read left ->right and returns True if valid
checkNorthSouth :: Clue -> Zed -> Bool
checkNorthSouth (n,e,s,w) zed = checkLeftToRight n (transposeZed zed)

-- Given clues and a zed board, maniplates input so that the East->West columns can be read left ->right and returns True if valid
checkEastWest :: Clue -> Zed -> Bool
checkEastWest (n,e,s,w) zed = checkLeftToRight e (reverseLists zed)

-- Given clues and a zed board, maniplates input so that the West->East columns can be read left ->right and returns True if valid
checkWestEast :: Clue -> Zed -> Bool
checkWestEast (n,e,s,w) zed = checkLeftToRight (reverseList w) zed

-- Given clues and a row of zed board, checks if each row is valid according to its respective clue
checkLeftToRight :: [Int] -> [[Int]] -> Bool
checkLeftToRight _ [] = True
checkLeftToRight [] _ = True
checkLeftToRight (c:ct) (r:rt)
 |(c == 0) = checkLeftToRight ct rt
 |checkNumberPostsVisited c r = checkLeftToRight ct rt
 |otherwise = False

-- Given a single clue and a single row, walks through the row and returns True if it is valid
checkNumberPostsVisited :: Int -> [Int] -> Bool
checkNumberPostsVisited c r = checkNumberPostsVisited' c r 0 0
{-
>checkNumberPostsVisited 3 [1,2,4,3]
True
>checkNumberPostsVisited 3 [1,2,3,4]
False
-}

checkNumberPostsVisited' :: Int -> [Int] -> Int -> Int -> Bool
checkNumberPostsVisited' clue [] tallest outposts
 |outposts == clue = True
 |otherwise = False

checkNumberPostsVisited' clue (h:t) tallest outposts
 |outposts>clue = False
 |h>tallest = checkNumberPostsVisited' clue t h (outposts+1)
 |h<tallest = checkNumberPostsVisited' clue t tallest outposts
 |h==tallest = False

--Grid Building----------------------------------------------------------------------------------------------------------------------

-- Given clues and a zed board, displays them as a grid in ghci
displayGrid (n,e,s,w) zed = putStr (toGrid (length n) (inputGrid (n,e,s,w) zed))

-- formats northmost row to display in grid
north :: Row -> Row
north row = [0] ++ row ++ [0]

-- formats west->east rows to display in ghci
weastHelper :: Int -> Int -> Row -> Row
weastHelper c1 c2 r = c1: (reverse(c2: reverse r))

-- formats west->east rows to display in ghci
weast :: Row -> Row -> Zed -> Zed
weast [] [] [] = []
weast (c1:c1t) (c2:c2t) (r:rt) =  (weastHelper c1 c2 r):(weast c1t c2t rt) 

-- formats southmost rows to display in ghci
south :: Row -> Row
south row = [0] ++ row ++ [0]

-- appends all rows together to display as grid
inputGrid :: Clue -> Zed -> Zed
inputGrid (n,e,s,w) z = reverse(reverse(south s) : (reverse ((north n) : (weast (reverse w) e z))))

-- shows grid in ghci
toGrid :: Int -> [[Int]] -> String
toGrid l = lnBreak l " | " . (map (map show))

-- adds new line characters to grid
lnBreak l str = intercalate ("\n" ++ (generateDash l) ++ "\n")  . map (intercalate str)

-- generates dash characters to display in between rows of grid
generateDash l = generateDash' (l*4+5)
generateDash' 0 = ""
generateDash' l = "-" ++ (generateDash' (l-1))
