#!/usr/bin/env runhaskell

-- index operator 
third a = a !! 2

-- let declares local variable in scope declared by 'in'
test = let l = ['a','b','c'] in head l

-- do block runs a series of IO operations
main = do 
  let l = [1,2,3]
  putStrLn "test"
  putStrLn $ show $ init l
  putStrLn $ show l
  putStrLn $ show test 
  putStrLn $ show $ third [1,2,3,4,5]

