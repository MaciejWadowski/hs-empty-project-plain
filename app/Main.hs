module Main where

import qualified Student
import qualified Data.ByteString.Lazy as B
import Data.Aeson

jsonFile :: FilePath
jsonFile = "file.json"

main :: IO ()
main = do
    let st = Student.Student "maciek" "w" 21
    putStrLn "Program konwertuje JSON"
    let zm =  encode $ st
    content <- B.readFile jsonFile
    let Just st = decode zm :: Maybe Student.Student
    print st