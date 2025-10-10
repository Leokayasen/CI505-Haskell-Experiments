# Basics of Haskell using GHCi

> [!WARNING]
> Remember, haskell files are always `.hs`
> Running scripts without the correct file extension will not work! 


## Basic Maths
```haskell
> 2 + 3
5

> 7 * 8
56

> 9 / 2
4.5
```

## Boolean Eval
```haskell
> True && False
False

> True || False
True

> not True
False
```

## Variables
```haskell
> let x = 10
> x * 2
20
```

## Variable Lists
```haskell
> let myList = [1, 2, 3]
> myList
[1, 2, 3,]

> length myList
3

> sum myList
6
```

## Manipulating Lists
```haskell
> head [1, 2, 3, 4, 5]
1

> reverse [1, 2, 3, 4, 5]
[5, 4, 3, 2, 1]

> tail [1, 2, 3, 4, 5]
[2, 3, 4, 5]

> [1, 2, 3, 4, 5] !! 3
4

> take 3 [1, 2, 3, 4, 5]
[1, 2, 3]

> drop 3 [1, 2, 3, 4, 5]
[4, 5]

> product [1, 2, 3, 4, 5]
120

> [1, 2, 3] ++ [4, 5]
[1, 2, 3, 4, 5]

> map (*2) [1, 2, 3, 4, 5]
[2, 4, 6, 8, 10]
```

## Defining Functions
```haskell
> let square n = n * n
> square 5
25

> square 10
100
```










