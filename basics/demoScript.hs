square x = x * x
cube x = x * x * x
factorial n = product [1..n]
average ns = sum ns `div` length ns

total x y = add x y
 where {add a b = a + b; square c = c * c}

add2 = (\x -> x + 2)
addToList :: [Int] -> [Int]
addToList x = map (add2) x

data Bool = True | False
data Colour = Red | Blue | Green
data Customer = Customer { id :: Int, name :: String, address :: [String], serviceLevel :: Colour }
