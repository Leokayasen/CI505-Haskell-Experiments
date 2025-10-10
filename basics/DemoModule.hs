module DemoModule (square, cube, factorial, average) where

square :: Int -> Int
square x = x * x
cube x = x * x * x
factorial n = product [1..n]
average ns = sum ns `div` length ns
