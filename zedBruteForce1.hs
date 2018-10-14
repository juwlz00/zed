module Zed where

import Data.List
import System.Random
import System.IO.Unsafe

--GIVEN CLUES AND POTENTIAL SOLUTION, RETURNS TRUE IF IT IS GOOD, ELSE FALSE
checkSolution clues zed = (checkValidNumbers zed) && (checkNotFools clues zed)

--GIVEN CLUES, SOLVES ZED BY GUESSING UNTIL IT GETS IT RIGHT. n>4 might take forever?                                  https://www.youtube.com/watch?v=9nazm3_OXac
zedBruteForce (n,s,e,w) = randomGuesser1 (n,s,e,w) (length n) (randomZed (length n) [] 0)
{-
*Zed> zedBruteForce ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])
[[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-}

randomGuesser1 a b c = randomGuesser2 a b c (permutations[1..b]) (factorial b)

randomGuesser2 clues n try perms fac
 |checkSolution clues try = try
 |otherwise = randomGuesser2 clues n (randomZed n perms fac) perms fac

randomZed n [] _ = [[]]
randomZed n perms fac = randomZed2 n n fac perms
randomZed2 n count fac perms
 |count>0 = perms !! unsafePerformIO (getStdRandom (randomR (0, fac-1)))  : randomZed2 n (count-1) fac perms
 |otherwise = []
 
factorial 0 = 1
factorial n = n * factorial (n - 1)

----------------------------

--intersect that handles duplicates https://stackoverflow.com/questions/27332815/haskell-intersection-with-duplicates
intersectD xs ys = xs \\ (xs \\ ys)

--CHECKS THAT EACH ROW/COLUMN HAS NUM 1-N
checkValidNumbers zed = (checkValidNumbers2 (transposeZed zed)) && (checkValidNumbers2 zed)

checkValidNumbers2 [] = True
checkValidNumbers2 (h:t) = (h `intersectD` [1..(length(h))] == h) && checkValidNumbers2 t

reverseList xs = foldl (\x y -> y:x) [] xs 

reverseLists [] = []
reverseLists (h:t) = (foldl (\x y -> y:x) [] h) : (reverseLists t)

transposeZed ([]:_) = []
transposeZed x = (map head x) : transposeZed (map tail x)

--CHECKS THAT EVERY ROW/COLUMN AGREES WITH THE CLUE
checkNotFools clues zed = checkSouthNorth clues zed && checkNorthSouth clues zed && checkEastWest clues zed && checkWestEast clues zed

checkSouthNorth (n,e,s,w) zed = checkLeftToRight (reverseList s) (reverseLists (transposeZed zed))
checkNorthSouth (n,e,s,w) zed = checkLeftToRight n (transposeZed zed)
checkEastWest (n,e,s,w) zed = checkLeftToRight e (reverseLists zed)
checkWestEast (n,e,s,w) zed = checkLeftToRight (reverseList w) zed

checkLeftToRight _ [] = True
checkLeftToRight [] _ = True
checkLeftToRight (c:ct) (r:rt)
 |(checkNumberPostsVisted c r 0 0) = checkLeftToRight ct rt
 |otherwise = False

checkNumberPostsVisted clue [] tallest outposts
 |outposts == clue = True
 |otherwise = False
checkNumberPostsVisted clue (h:t) tallest outposts
 |outposts>clue = False
 |h>tallest = checkNumberPostsVisted clue t h (outposts+1)
 |h<tallest = checkNumberPostsVisted clue t tallest outposts
 |h==tallest = False







