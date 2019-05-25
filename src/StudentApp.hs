module StudentApp where

import System.Exit

data Student = Student {name::String, age::Int, id::String}
  deriving (Show, Eq, Read)

mainLoop :: [Student] -> IO [Student]
mainLoop students = do
    putStrLn "1. dodaj nowego studenta, \n2. wyświetl wszystkich studentów, \n3. usuń studenta o zadanym numerze albumu, \n4. zakończ pracę z programem"
    c <- fmap read getLine
    let a = chooseAction c
    students' <- a students
    mainLoop students'

chooseAction :: Int -> Action
chooseAction userchoice
    | userchoice == 1 = addStudent
    | userchoice == 2 = printStudents
    | userchoice == 3 = deleteStudents
    | otherwise = quit


type Action = [Student] -> IO [Student]

addStudent :: Action
addStudent xs = do
    name <- getLine
    age <-  fmap read getLine
    id <- getLine
    return ((Student name age id):xs)

printStudents :: Action
printStudents [] = do
    putStrLn "empty list \n"
    return ([])
printStudents xs = do
    mapM print (fmap show xs)
    return (xs)

deleteStudents :: Action
deleteStudents xs = do
    id <- getLine
    return (delete id xs)

delete :: String -> [Student] -> [Student]
delete id [] = []
delete id ((Student name age id'):xs)
    | id == id' = xs
    | otherwise = (Student name age id'):(delete id xs)

quit :: Action
quit _ = exitFailure