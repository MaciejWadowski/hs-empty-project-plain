module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sum' :: (Num a) => [a] -> a
sum' x = foldl (+) 0 x

product' :: (Num a) => [a] -> a
product' x = foldl (*) 1 x

and' :: [Bool] -> Bool
and' x = foldl (&&) True x

or' :: [Bool] -> Bool
or' x = foldl (||) False x