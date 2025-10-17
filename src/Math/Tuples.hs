module Math.Tuples (
    swapTuple,
    tupleSum
) where

-- | Swap elements of a tuple
swapTuple :: (a, b) -> (b, a)
swapTuple (x, y) = (y, x)

-- | Sum elements of a tuple
tupleSum :: Num a => (a, a) -> a
tupleSum (x, y) = x + y