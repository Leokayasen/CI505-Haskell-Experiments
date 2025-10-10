## Scripts
You can write a haskell script using any text editor and using file extension `.hs`
```haskell
square x = x * x
cube x = x * x * x
```
To interact with the functions, you can use the following GHCi command:
`> ghci test.hs`

This loads the standard library + the functions you wrote in `test.hs`
```haskell
> square 10
100

> cube 2
8
```

### Other custom definitions in test.hs
```haskell
factorial n = product [1..n]
average ns = sum ns `div` length ns
```
Using \` ` denotes infix operators when defining functions.
