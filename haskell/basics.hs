#!/usr/bin/env runhaskell

module Examples 
-- functions to export
( third
, initials
  -- use two docs to export a type with it's constructors
  -- A(..)
) where

-- notes and examples mostly from learn you a haskell

-- all exported functions available by import of package
--   rename import with 'as'; exclude funs with 'hiding'
import qualified Data.List as M hiding (nub) 
import Data.Map hiding (map)
import Data.Char
import System.IO
import Control.Monad

data Person = Person String String

firstName :: Person -> String
firstName (Person _ firstName) = firstName

lastName :: Person -> String
lastName (Person _ lastName) = lastName

-- record syntax (creates the same as above so fields must have different names)
data PersonRecord = PersonRecord { 
  first :: String,
  last :: String 
-- deriving creates type classes instances for this data type
} deriving (Eq, Show, Read)

-- type alias
type A = Person

-- repl commands: 
--  use :t to check types
  -- infix functions need to be surrounded with parentheses 
-- import module: :m + Data.List

-- index operator 
third a = a !! 2

-- let declares local variable in scope declared by 'in'
--   - let bindings do no span across guards
letBinding = let l = ['a','b','c'] in head l
-- let expressions can declare variables and functions in inline
letExpression = (let i = 2 in i + 10) + 5

-- $ is function application with lowest precedence 
--   application with spaces is left-associative, while with $ it is 
--   right-associative

-- . is used for function compisition
-- f(g(x)) is the same as (f . g) x and f . g $ x

-- do block runs a series of IO operations
main = do 
  let l = [1,2,3]
  putStrLn "test"
  putStrLn $ show $ letExpression
  putStrLn $ show $ init l
  putStrLn $ show l
  putStrLn $ show letBinding
  putStrLn $ show $ third [1,2,3,4,5]
  putStrLn $ show [1..20]
  putStrLn $ show ['a'..'z']
  -- ranges can define step size
  putStrLn $ show [5,10..50]
  -- infinite ranges also possible
  putStrLn $ show $ take 10 [0..]
  -- cycle repeats a list infinitely
  putStrLn $ show $ take 5 $ cycle [1,2]
  -- repeat does the same for other types
  putStrLn $ show $ take 5 $ repeat 1
  -- more concise with replicate
  putStrLn $ show $ replicate 5 1
  putStrLn $ show [x * 2 | x <- [1..10]]
  -- list membership test
  putStrLn $ show $ elem 'a' "abc"
  -- fst and snd are used to access members of pairs
  putStrLn $ show $ snd $ fst ((1, 2), 3)
  -- read parses a type
  putStrLn $ show $ read "10" + 2
  -- use type annotations if inference is not possible
  putStrLn $ show $ (read "12" :: Int)
  -- bounded type class 
  putStrLn $ show $ (minBound :: Int)
  putStrLn $ show $ (maxBound :: Int)
  -- \ declares a lambda
  putStrLn $ show $ map (\(a, b) -> a + b) [(0, 1), (1, 2), (2, 3)]  

-- tuples are limited to 64 elements

-- Int vs Integer - Int is bounded, Integer unbounded (like BigInt)

max' a b
  -- guards allow branching on boolean conditions
  | a <= b = b
  -- fallthrough
  | otherwise = a

initials first last = [f] ++ "." ++ [l] ++ "."
  -- where clause defines variables visible only to the function
  where (f:_) = first
        (l:_) = last

map' :: (a -> b) -> [a] -> [b]
-- pattern match across function bodies
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- IO

yell = forever $ do 
  l <- getLine
  putStrLn $ map toUpper l

yell' = do 
  -- getContents is lazy version of getLine
  l <- getContents
  putStr $ map toUpper l

-- interact takes a function to map over the provided input
yell'' = interact $ map toUpper

-- use file instead of standard out
yell''' file = do 
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  putStr $ map toUpper contents
  hClose handle

-- simplify using withFile
yell'''' file = do
  withFile file ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr $ map toUpper contents)

yell''''' file = do
  contents <- readFile file
  putStr $ map toUpper contents

