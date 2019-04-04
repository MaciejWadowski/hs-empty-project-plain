module Student where

data Student = Student {
               firstName::String,
               lastName::String,
               age::Int} deriving (Show, Read, Eq)

fullName :: Student -> String
fullName student = firstName student ++ " " ++ lastName student

type SortedStudents =  [(Int, Student)]

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
studentConvert' list stud = list ++ [((length list)+1, stud)]

studentsToString :: String -> SortedStudents -> String
studentsToString str () =  