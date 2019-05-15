module Tree where

import Control.Applicative

data Tree a = EmptyTree
              | Node a (Tree a) (Tree a)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

instance (Eq a) => (Eq (Tree a)) where 
    (Node val1 left1 right1) == (Node val left right) = (val == val1) && (left == left1) && (right == right1)
    EmptyTree == EmptyTree = True
    _ == _ = False
    (Node val1 left1 right1) /= (Node val left right) = (val /= val1) || (left /= left1) || (right /= right1)
    EmptyTree /= EmptyTree = False
    _ /= _ = True

instance (Show a) => (Show (Tree a)) where
    show (Node a EmptyTree EmptyTree) = " Node " ++ show a ++ " EmptyTree EmptyTree "
    show (Node a left EmptyTree) = " Node " ++ show a ++ " (" ++ show left ++ ") EmptyTree "
    show (Node a EmptyTree right) = " Node " ++ show a ++ " EmptyTree (" ++ show right ++ ") "
    show (Node a left right) = " Node " ++ show a ++ " (" ++ show left ++ " " ++ show right ++ ") "

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance Applicative Tree where
    pure x = Node x EmptyTree EmptyTree
    (Node f leftf rightf) <*> (Node a left right) = Node (f a)  (leftf <*> left)  (rightf <*> right)
    _ <*> _ = EmptyTree
--    liftA2 f (Node a left1 right1) (Node b left2 right2) = Node (f a b) (liftA2 left1 left2) (liftA2 right1 right2)
--    liftA2 f (Node a left right) _ = Node (f a b) left right
--    liftA2 f _ (Node a left right) = Node (f a b) left right
--    liftA2 f _ _ = EmptyTree

insert :: (Ord a) => a -> (Tree a) -> (Tree a)
insert x EmptyTree = singleton x
insert x (Node a left right)
      | x == a = Node x left right
      | x > a =  Node a left (insert x right)
      | x < a = Node a (insert x left) right

empty :: (Tree a) -> Bool
empty EmptyTree = True
empty (Node a left right) = False

isBinary :: (Tree a) -> Bool
isBinary EmptyTree = True
isBinary (Node a left right) = True

search :: (Ord a) => a -> (Tree a) -> Bool
search x EmptyTree = False
search x (Node a left right)
       | x == a = True
       | x > a = search x right
       | x < a = search x left

nnodes :: (Tree a) -> Int
nnodes EmptyTree = 0
nnodes (Node a left right) = 1 + (nnodes left) + (nnodes right)

isBalanced :: (Tree a) -> Bool
isBalanced EmptyTree = True
isBalanced (Node a left right) = (abs ((nnodes left) - (nnodes right))) <= 1

traverseTree :: String -> (Tree a) -> [a]
traverseTree x EmptyTree = []
traverseTree x (Node a left right)
          | x == "LVR" = (traverseTree x left) ++ [a] ++ (traverseTree x right)
          | x == "VLR" = [a] ++ (traverseTree x left) ++ (traverseTree x right)
          | x == "LRV" = (traverseTree x left) ++ (traverseTree x right) ++ [a]
          | x == "VRL" = [a] ++ (traverseTree x right) ++ (traverseTree x left)
          | x == "RVL" = (traverseTree x right) ++ [a] ++ (traverseTree x left)
          | x == "RLV" = (traverseTree x right) ++ (traverseTree x left) ++ [a]
          | otherwise = error "Invalid String Argument"

toString :: (Show a) => (Tree a) -> String
toString EmptyTree = ""
toString (Node a left right) = (show a) ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"

leaves :: (Eq a) => (Tree a) -> [a]
leaves EmptyTree = []
leaves (Node a EmptyTree EmptyTree) = [a]
leaves (Node a EmptyTree right) = leaves right
leaves (Node a left EmptyTree) = leaves left
leaves (Node a left right) = leaves left ++ leaves right

nsum :: (Num a) => (Tree a) -> a
nsum EmptyTree = 0
nsum (Node a left right) = a + (nsum left) + (nsum right)

tmap :: (a -> b) -> (Tree a) -> (Tree b)
tmap x EmptyTree = EmptyTree
tmap x (Node a left right) = Node (x a) (tmap x left) (tmap x right)

findMin :: (Ord a) => (Tree a) -> a
findMin (Node a left right)
    | left == EmptyTree = a
    | otherwise = findMin left

remove :: (Ord a) => a -> (Tree a) -> (Tree a)
remove x EmptyTree = EmptyTree
remove x (Node a left right)
        | x < a = Node a (remove x left) right
        | x > a = Node a left (remove x right)
        | x == a && left == EmptyTree && right == EmptyTree = EmptyTree
        | x == a && left == EmptyTree = right
        | x == a && right == EmptyTree = left
        | x == a = Node (findMin right) left (remove (findMin right) right)

merge :: (Ord a) => (Tree a) -> (Tree a) -> (Tree a)
merge EmptyTree EmptyTree = EmptyTree
merge x EmptyTree = x
merge EmptyTree x = x
merge left right = merge (insert (findMin right) left) (remove (findMin right) right)
