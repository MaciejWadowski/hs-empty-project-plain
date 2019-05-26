module Db where


import Database.HDBC
import Database.HDBC.Sqlite3
import System.Exit
data Student = Student {firstName::String, lastName::String, age::Int}
  deriving (Show,Eq,Read)

type Action = Connection -> IO ()

main' :: IO ()
main' = do
   connection <- connectSqlite3 "test.db"
   state <- prepare connection "CREATE TABLE IF NOT EXISTS students (first_name TEXT PRIMARY KEY, last_name Text NOT NULL, age INTEGER NOT NULL);"
   execute state []
   putStrLn "1. dodaj nowego studenta\n2. wyświetl wszystkich studentów\n3. usuń studenta o zadanym numerze albumu\n4.Zaktualizuj studenta w bzie \n Cokolwiek - zakończ pracę z programem"
   mainLoop connection
   return ()

mainLoop :: Connection -> IO ()
mainLoop connection = do
    c <- fmap read getLine
    let a = chooseAction c
    a connection
    mainLoop connection

chooseAction :: Int -> Action
chooseAction action
    | action == 1 = addStudent
    | action == 2 = printStudent
    | action == 3 = deleteStudent
    | action == 4 = updateStudent
    | otherwise = quit

studentToSql :: Student -> [SqlValue]
studentToSql (Student name lastName age) = [toSql name, toSql lastName, toSql age]

updateStudent :: Action
updateStudent connection = do
    putStrLn "Podaj imie"
    name <- getLine
    putStrLn "Podaj nazwisko"
    lastName <-  getLine
    putStrLn "Podaj wiek"
    age <- fmap read getLine
    putStrLn "---------\nPodaj zaktualizowane dane\nPodaj imie"
    newName <- getLine
    putStrLn "Podaj nazwisko"
    newLastName <-  getLine
    putStrLn "Podaj wiek"
    newAge <- fmap read getLine
    state <- prepare connection "UPDATE students SET first_name=?, last_name=?, age=? WHERE first_name=? AND last_name=? AND age=?"
    let oldStud = Student name lastName age
        newStud = Student newName newLastName newAge
    execute state ((++) (studentToSql newStud) (studentToSql oldStud))
    return ()

deleteStudent :: Action
deleteStudent connection = do
    putStrLn "Podaj imie"
    name <- getLine
    putStrLn "Podaj nazwisko"
    lastName <-  getLine
    putStrLn "Podaj wiek"
    age <- fmap read getLine
    let stud = Student name lastName age
    state <- prepare connection "DELETE FROM students WHERE first_name=? AND last_name=? AND age=?;"
    execute state (studentToSql stud)
    return ()

printStudent :: Action
printStudent connection = do
    select <- prepare connection "SELECT * FROM students;"
    execute select []
    result <- fetchAllRows select
    putStrLn $ show result
    return()

addStudent :: Action
addStudent connection = do
   putStrLn "Podaj imie"
   name <- getLine
   putStrLn "Podaj nazwisko"
   lastName <-  getLine
   putStrLn "Podaj wiek"
   age <- fmap read getLine
   let stud = Student name lastName age
   state <- prepare connection "INSERT INTO students values(?,?,?);"
   execute state (studentToSql stud)
   return ()

quit :: Action
quit connection = do
    commit connection
    disconnect connection
    exitSuccess