# Week 4 of Haskell - I/O

## Understanding IO actions

```haskell
main :: IO()
main = putStrLn "Hello World"
```

- `IO()` is a type for I/O actions that do not return a useful value (like `void` in Java)
- `IO()` actions can return or contain values of any type: boolean, string, int
- In the example above, `putStrLn` is a string type for `IO()`
- Once a value is encapsulated, it cannot be used in pure functions.


## Interacting with users

```haskell
main :: IO()
main = do putStrLn "Hi, what's your name?"
          name <- getLine
          putStrLn ("Ok, nice to meet you " ++ name)
```

- I/O actions can be tied together using the `do` notation.
- Each line within `do` is a separate I/O action
  - Entire `do` block itself is also considered an I/O action and its value is determined by the value of the last line in the block
- `getLine` retrieves a line of input from the user.
- The `<-` operator binds the retrieved value to the identifier `name`


## Integrating Pure Functions with IO actions

```haskell
helloString :: String -> String
helloString n = "OK, nice to meet you " ++ n

main :: IO ()
main = do putStrLn "Hi, what's your name?"
          name <- getLine
          putStrLn (helloString name)

```

- Calling a pure function from within an IO action is simple, as shown above.
- `helloString` is a pure function in Haskell.


## Working with IO actions

```haskell
helloString :: String -> String
helloString n = "OK, nice to meet you " ++ n

sayHello :: String -> IO()
sayHello str = putStrLn str

main :: IO ()
main = do putStrLn "Hi, what's your name?"
          name <- getLine
          sayHello (helloString name)
```

- IO actions can be defined, stored and passed around within the program without immediate execution.
- These are only executed within another IO block.
- `sayHello` is an IO action that prints a string to the console.


## Calling pure code from within an IO action

```haskell
helloString :: String -> String
helloString n = "OK, nice to meet you " ++ n

sayHello :: String -> IO()
sayHello str = putStrLn str

main :: IO ()
main = do putStrLn "Hi, what's your name?"
          name <- getLine
          let str = helloString name
          sayHello str
```

- To store the result of a pure function within an IO action, use `let`
- Unlike a purely functional context, you don't need `in` after the let statement.
- In an IO action, you use `<-` to extract a value from an IO action and `let` to bind a value.


## Return

```haskell
main :: IO ()
main = do line <- getLine
          if null line
          then return ()
          else do putStrLn (reverseWords line)
               main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

- `return` is a function, not a keyword.
- It wraps its input in a IO action.
- In the above program, execution of the program is stopped if the user enters an empty line.

> [!TIP]
> `return` does not mark the end of a function and might not be the last expression to be evaluated in an IO action.


## Working with actions

```haskell
import Control.Monad

-- | echo input until the input is a space
main :: IO()
main = do
   c <- getChar
   when (c /= ' ') (do
     putChar c
     main)
```

- The `Control.Monad` module offers several useful functions for working with actions.
- One example is `when` which takes a predicate and an IO action, then performs the action if the predicate is true.


```haskell
import Control.Monad
import Data.Char

main :: IO ()
main = forever (do
         putStr "Give me some input: "
         l <- getLine
         putStrLn (map toUpper l))
```

- `forever` takes an IO action and performs it forever.

```haskell
import System.Environment
import System.IO
import System.IO.Error

main :: IO()
main = toTry `catch` handler

toTry :: IO()
toTry = do (fileName:_) <- getArgs
          contents <- readFile fileName
          putStrLn ("The file has " ++ show (length (lines contents)) ++ " lines!")

handler ::IOError -> IO()
handler e = putStrLn "Whhops, had some trouble!"
```

- `catch` takes two IO actions, tries perform one, and performs the second if the first causes an exception.

```haskell
main = do
     a <- getLine
     b <- getLine
     c <- getLine
     print [a,b,c]

-- | this is the same as

main = do
     rs <- sequence [getLine, getLine, getLine]
     print rs
```

- `sequence` takes a list of IO actions and returns an IO action that performs them one after the other.

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]

ghci> mapM_ print [1,2,3]
1
2
3
```

- `mapM` encapsulates the common problem of mapping an IO action over an input list, sequencing it or performing all actions.
- `mapM_` does the same, and throws result after.


## Command Line IO

```haskell
putStr :: String -> IO ()
```

- `putStr` prints a String without appending a newline character

```haskell
putChar :: Char -> IO ()
```

- `putChar` prints a single character

```haskell
print :: Show a => a -> IO()
```

- `print` prints a string representation of any instance of `Show`

```haskell
getChar :: IO() Char
```

- `getChar` reads a single character from stdin

```haskell
getContents :: IO String
```

- `getContents` reads from stdin until reaching the EOF character























