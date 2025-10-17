module Math.Lists (
    listSum,
    listProduct,
    pairwise,
    squaresOfEvens,
    allPairs,
    result,
    list1,
    merge1,
    merge2,
    mergeResult,
    unfilteredList,
    filterResult,
    pairedTuple,
    pairedResult,
    filterPair,
    filterPairResult
) where

-- | Sum all elements in a list
listSum :: Num a => [a] -> a
listSum = sum

-- | Compute the product of all elements in a list
listProduct :: Num a => [a] -> a
listProduct = product

-- | Combine a list into pairs: [a,b,c,d] -> [(a,b),(c,d)]
pairwise :: [a] -> [(a,a)]
pairwise (x:y:rest) = (x, y) : pairwise rest
pairwise _ = []

-- | Squares of even numbers from a list
squaresOfEvens :: Integral a => [a] -> [a]
--squaresOfEvens xs = [x^2 | x <- xs, even x]
squaresOfEvens xs = map (^2) (filter even xs)

-- | All possible pairs from two lists
allPairs :: [a] -> [b] -> [(a,b)]
-- Old Method:
-- allPairs xs ys = [ (x,y) | x <- xs, y <- ys ]
-- New Method:
allPairs xs ys = concatMap (\x -> map (\y -> (x,y)) ys) xs

-- | List Comprehensions
list1 :: [Int]
list1 = [1,2,3,4,5]

result :: [Int]
-- result = [ x + 1 | x <- list1 ]
result = map (+1) list1

merge1 :: [Int]
merge1 = [1,2,3,4,5]

merge2 :: [Int]
merge2 = [3,4,5,6,7]

mergeResult :: [Int]
-- mergeResult = [ x + y | x <- merge1, y <- merge2 ]
mergeResult = concatMap (\x -> map (\y -> x + y) merge2) merge1

-- | Filtered Comprehension
unfilteredList :: [Int]
unfilteredList = [1,3,5,7]

filterResult :: [Int]
-- filterResult = [ x + 2 | x <- unfilteredList, x > 4 ]
filterResult = map (+2) (filter (>4) unfilteredList)

-- | Paired
pairedTuple :: [(Int, Int)]
pairedTuple = [(1,10), (2,20), (3,30)]

pairedResult :: [Int]
-- pairedResult = [ x + 3 | (x, _) <- pairedTuple]
pairedResult = map (\(x, _) -> x + 3) pairedTuple

-- | Filtered Pair
filterPair :: [(Int, Int)]
filterPair = [(1,2), (2,2), (3,4), (4,1), (5,6)]

filterPairResult :: [Int]
-- filterPairResult = [ x + 5 | (x,y) <- filterPair, x + y < 6 ]
filterPairResult = map (\(x,y) -> x + 5) (filter (\(x,y) -> x + y < 6) filterPair)
