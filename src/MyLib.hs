module MyLib where

import qualified Data.Map as Map
import qualified Data.Heap as Heap

type Result = [(String, Int)]
type WordList = [String]

processFile :: String -> Result
processFile fileContent = toResult . selectMostFrequent 10 . countWords . toWordList $ fileContent

processWords :: WordList -> Char -> WordList
processWords list ' ' = "" : list
processWords (x:xs) char = (x ++ [char]):xs
processWords [] char = [[char]]

toWordList :: String -> [String]
toWordList longSentence = foldl processWords [] longSentence

countWords :: [String] -> Map.Map String Int
countWords list = countWords' list Map.empty

fromJust :: Maybe a -> a
fromJust (Just a) = a

countWords' :: [String] -> Map.Map String Int -> Map.Map String Int
countWords' [] map = map
countWords' (x:xs) map
       | Map.member x map == True = countWords' xs (Map.insert x (((fromJust (Map.lookup x map))+1)) map)
       | otherwise = countWords' xs (Map.insert x 1 map)

selectMostFrequent :: Int -> Map.Map String Int -> Heap.MinHeap (String, Int)
selectMostFrequent n map = selectMostFrequent' n (Map.toList map) Heap.empty

selectMostFrequent' :: Int -> [(String, Int)]-> Heap.MinHeap (String, Int) -> Heap.MinHeap (String, Int)
selectMostFrequent' n list heap
    | n == 0 = heap
    | n > 0 = selectMostFrequent' (n-1) (removeItem list maxVal) (Heap.insert maxVal heap)
    where maxVal = foldl maximums (head list) list

removeItem :: [(String, Int)] -> (String, Int)-> [(String, Int)]
removeItem ((x1,x2):xs) (y1,y2)
    | x1 == y1 = xs
    | otherwise = (x1,x2): removeItem xs (y1,y2)

maximums :: (String, Int) -> (String, Int) -> (String, Int)
maximums (x1,x2) (y1,y2)
    | x2 > y2 = (x1,x2)
    | otherwise = (y1,y2)

toResult :: Heap.MinHeap (String, Int) -> Result
toResult heap = Heap.toList heap