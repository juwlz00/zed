module Zed where

import Data.List
import System.Random
import System.IO.Unsafe
import Control.Monad

type Row = [Int]
type Zed = [Row]
type Clue = ([Int],[Int],[Int],[Int])


-- Given clues, returns a solved zed board
solveZed :: Clue -> Zed             
solveZed (n,s,e,w) = randomGuesser (n,s,e,w) (length n) (randomZed (length n) [] 0) (permutations[1..(length n)]) (factorial (length n))

-- Given clues, returns a solved zed board displayed as a grid
solveZedGrid clues = displayGrid clues (solveZed clues)

--Given clues and a zed board, returns true if the zed board is correct
checkSolution :: Clue -> Zed -> Bool
checkSolution clues zed = (checkValidNumbers zed) && (checkNotFools clues zed)

--Randomly guesses permutations of zed boards until a solution is found
randomGuesser clues n try perms fac
 |checkSolution clues try = try
 |otherwise = randomGuesser clues n (randomZed n perms fac) perms fac

--Generates a random zed board
randomZed n [] _ = [[]]
randomZed n perms fac = randomZed2 n n fac perms
randomZed2 n count fac perms
 |count>0 = perms !! unsafePerformIO (getStdRandom (randomR (0, fac-1)))  : randomZed2 n (count-1) fac perms
 |otherwise = []

factorial 0 = 1
factorial n = n * factorial (n - 1)


-----------------------------------------------------------------------------------------------------------------------------------------

-- Given a zed board, checks that each row & column contains the numbers 1-n
checkValidNumbers zed = (checkValidNumbers2 (transposeZed zed)) && (checkValidNumbers2 zed)

--intersect that handles duplicates https://stackoverflow.com/questions/27332815/haskell-intersection-with-duplicates
intersectD xs ys = xs \\ (xs \\ ys)

checkValidNumbers2 [] = True
checkValidNumbers2 (h:t) = (h `intersectD` [1..(length(h))] == h) && checkValidNumbers2 t

reverseList xs = foldl (\x y -> y:x) [] xs 

reverseLists [] = []
reverseLists (h:t) = (foldl (\x y -> y:x) [] h) : (reverseLists t)

transposeZed ([]:_) = []
transposeZed x = (map head x) : transposeZed (map tail x)

-- Given clues and a zed board, returns True if the merchants are not fools (they only go to outposts if they are better than previous outposts)
checkNotFools :: Clue -> Zed -> Bool
checkNotFools clues [[]] = False
checkNotFools clues zed = checkSouthNorth clues zed && checkNorthSouth clues zed && checkEastWest clues zed && checkWestEast clues zed

-- Given clues and a zed board, maniplates input so that the South->North columns can be read left ->right
checkSouthNorth :: Clue -> Zed -> Bool
checkSouthNorth (n,e,s,w) zed = checkLeftToRight (reverseList s) (reverseLists (transposeZed zed))

-- Given clues and a zed board, maniplates input so that the North->South columns can be read left ->right
checkNorthSouth :: Clue -> Zed -> Bool
checkNorthSouth (n,e,s,w) zed = checkLeftToRight n (transposeZed zed)

-- Given clues and a zed board, maniplates input so that the East->West columns can be read left ->right
checkEastWest :: Clue -> Zed -> Bool
checkEastWest (n,e,s,w) zed = checkLeftToRight e (reverseLists zed)

-- Given clues and a zed board, maniplates input so that the West->East columns can be read left ->right
checkWestEast :: Clue -> Zed -> Bool
checkWestEast (n,e,s,w) zed = checkLeftToRight (reverseList w) zed

-- Given clues and a row of zed board, checks if each row is valid according to its respective clue
checkLeftToRight :: [Int] -> [[Int]] -> Bool
checkLeftToRight _ [] = True
checkLeftToRight [] _ = True
checkLeftToRight (c:ct) (r:rt)
 |(c == 0) = checkLeftToRight ct rt
 |checkNumberPostsVisted c r = checkLeftToRight ct rt
 |otherwise = False

-- Given a single clue and a single row, walks through the row and returns True if it is valid
checkNumberPostsVisted :: Int -> [Int] -> Bool
checkNumberPostsVisted c r = checkNumberPostsVisted' c r 0 0
 
checkNumberPostsVisted' :: Int -> [Int] -> Int -> Int -> Bool
checkNumberPostsVisted' clue [] tallest outposts
 |outposts == clue = True
 |otherwise = False

checkNumberPostsVisted' clue (h:t) tallest outposts
 |outposts>clue = False
 |h>tallest = checkNumberPostsVisted' clue t h (outposts+1)
 |h<tallest = checkNumberPostsVisted' clue t tallest outposts
 |h==tallest = False


--Grid Building----------------------------------------------------------------------------------------------------------------------------

north :: Row -> Row
north row = [0] ++ row ++ [0];

weastHelper :: Int -> Int -> Row -> Row
weastHelper c1 c2 r = c1: (reverse(c2: reverse r))

weast :: Row -> Row -> Zed -> Zed
weast [] [] [] = [];
weast (c1:c1t) (c2:c2t) (r:rt) =  (weastHelper c1 c2 r):(weast c1t c2t rt) 

south :: Row -> Row
south row = [0] ++ row ++ [0];

inputGrid :: Zed -> Clue -> Zed
inputGrid z (n,e,s,w) = reverse(reverse(south s) : (reverse ((north n) : (weast (reverse w) e z))));

displayGrid (n,e,s,w) zed = putStr (toGrid2 (length n) (inputGrid zed (n,e,s,w)))

toGrid2 :: Int -> [[Int]] -> String
toGrid2 l = lnBreak l " | " . (map (map show))

--lnBreak :: Int -> [[String]] -> String
lnBreak l str = intercalate ("\n" ++ (generateDash l) ++ "\n")  . map (intercalate str)

generateDash l = generateDash' (l*4+5)
generateDash' 0 = ""
generateDash' l = "-" ++ (generateDash' (l-1))

--test case
--putStr (toGrid2 [[1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]])

