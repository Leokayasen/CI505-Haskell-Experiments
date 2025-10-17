module Main (main) where

import Utils
import Math.Arithmetic
import Math.Geometry
import Math.Tuples
import Math.Lists

main :: IO ()
main = do
    putStrLn "Testing Utils"
    print(double 5)
    print(square 3)
    print(cube 2)
    putStrLn "- - -"

    putStrLn "Testing Math.Arithmetic"
    print $ safeDiv 10 2
    print $ safeDiv 10 0
    print $ custRound 3.7
    putStrLn "- - -"

    putStrLn "Testing Math.Geometry"
    print $ areaCircle 2
    print $ perimeterCircle 2
    putStrLn "- - -"

    putStrLn "Testing Tuples & Lists"
    print $ swapTuple (1, "hello")
    print $ tupleSum (1, 2)
    print $ listSum [1,2,3,4]
    print $ pairwise [1,2,3,4,5,6,7,8]
    print $ squaresOfEvens [1..10]
    print $ allPairs [1,2] ['a', 'b']
    putStrLn "- - -"

    putStrLn "Testing & Rewriting List Comprehensions"
    putStrLn "Adding 1 to all elements of a list"
    print $ list1
    print $ result

    putStrLn "Merging two Lists"
    print $ merge1
    print $ merge2
    print $ mergeResult

    putStrLn "Filtered Comprehension"
    print $ unfilteredList
    print $ filterResult

    putStrLn "Paired Comprehension"
    print $ pairedTuple
    print $ pairedResult

    putStrLn "Filtered Pair Comprehension"
    print $ filterPair
    print $ filterPairResult



