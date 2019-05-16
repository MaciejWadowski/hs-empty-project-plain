{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Student where
import Data.Aeson
import Control.Applicative
import qualified Data.Text as T

data Student = Student {
               firstName::T.Text,
               lastName::T.Text,
               age::Int} deriving (Show, Read)

fullName :: Student -> T.Text
fullName student = firstName student `T.append` " " `T.append` lastName student

type SortedStudents =  [(Int, Student)]

instance Eq Student where 
    (Student fn ln a) == (Student fn1 ln1 a1) = (fn == fn1) && (ln == ln1) && (a == a1)
    (Student fn ln a) /= (Student fn1 ln1 a1) = (fn /= fn1) || (ln /= ln1) || (a == a1)


setAge :: Int -> Student -> Student
setAge x (Student firstName lastName age) = Student firstName lastName x

listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]

studentConvert :: Student -> Int -> (Int, Student)
studentConvert stud number = (number, stud)

--map fullName listToProcess

mapToTuple :: [Student] -> Int -> [(Student,Int)]
mapToTuple (x:xs) b = [(x,b)] ++ mapToTuple xs (b+1)
mapToTuple [] b = []

studentConvert' :: SortedStudents -> Student -> SortedStudents
studentConvert' list stud = list ++ [((Prelude.length list)+1, stud)]
{-
formatStudents :: SortedStudents -> T.Text
formatStudents ((x1,x2):xs) = show x1 `T.append` ". student: " `T.append` (lastName x2) `T.append` " "  `T.append` [Prelude.head (firstName x2) ]`T.append` ". wiek " `T.append` show (age x2) `T.append` "\n" `T.append` formatStudents xs
formatStudents [] = []

convertToHtml :: [Student] -> T.Text
convertToHtml list =  "<table>\n  <tr>\n    <th>FirstName</th>\n    <th>LastName</th>\n    <th>Age</th>\n  </tr>\n" `T.append` convertSortedStudents list `T.append` "</table>\n"

convertSortedStudents :: [Student] -> T.Text
convertSortedStudents [] = []
convertSortedStudents (x:xs) = "  <tr>\n   <td>" `T.append` firstName x ++ "</td>\n    <td>" ++ lastName x ++ "</td>\n    <td>" ++ show (age x) ++ "</td>\n  </tr>\n" ++ convertSortedStudents xs -}

instance FromJSON Student where
    parseJSON (Object s) =
        Student     <$> s .: "firstName"
                    <*> s .: "lastName"
                    <*> s .: "age"

instance ToJSON Student where
    toJSON (Student first last agee) =
         object [ "firstName" .= first
                , "lastName"  .= last
                , "age"       .= agee
                ]