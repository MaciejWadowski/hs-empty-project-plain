module MyLib where

import qualified Data.Map as Map
import qualified Data.Heap as Heap

type Result = [(String, Int)]
type WordList = [String]

processFile :: String -> Result
processFile fileContent = toResult . selectMostFrequent 10 . countWords . toWordList $ fileContent

processWords :: WordList -> Char -> WordList
processWords list ' ' = "" : list
processWords list '\n' = "" : list
processWords (x:xs) char = (x ++ [char]):xs
processWords [] char = [[char]]

toWordList :: String -> [String]
toWordList longSentence = foldl processWords [] longSentence

countWordsFoldl :: Map.Map String Int -> String -> Map.Map String Int
countWordsFoldl map word
        | Map.member word map == True = Map.insert word (fromJust (Map.lookup word map) + 1) map
        | otherwise = Map.insert word 1 map

countWords :: [String] -> Map.Map String Int
countWords list = foldl countWordsFoldl Map.empty list

fromJust :: Maybe a -> a
fromJust (Just a) = a

selectMostFrequentFoldl :: Heap.MinHeap (String, Int) -> (String, Int)  -> Heap.MinHeap (String, Int)
selectMostFrequentFoldl heap (word, count)
    | count > intVal = Heap.insert (word,count) (Heap.drop 1 heap)
    | otherwise = heap
    where (str, intVal) = fromJust (Heap.viewHead heap)

selectMostFrequent :: Int -> Map.Map String Int -> Heap.MinHeap (String, Int)
selectMostFrequent n map = foldl selectMostFrequentFoldl (Heap.fromList (take n list)) list
        where list = Map.toList map

toResult :: Heap.MinHeap (String, Int) -> Result
toResult heap = Heap.toList heap