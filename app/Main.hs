module Main where

import qualified KMers

main :: IO ()
main = do
    putStrLn "Program wypisuje ilosc slow z pliku"
    content <- readFile "ecoli.fa"
    let result = KMers.processFile content
    print result
