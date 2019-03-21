module Student where

data Student = Student {
               firstName::String,
               lastName::String,
               age::Int} deriving (Show)

fullName :: Student -> String
fullName student = firstName student ++ " " ++ lastName student
{-
instance Show Student where
    show (Student firstName lastName age) =
        "Student " ++ firstName ++ " " ++ lastName ++ " wiek " ++ show age
-}
setAge :: Int -> Student -> Student
setAge x (Student firstName lastName age) = Student firstName lastName x