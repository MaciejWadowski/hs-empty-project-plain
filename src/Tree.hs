module Tree where

data Tree a = EmptyTree
              | Node a (Tree a) (Tree a)
              deriving(Show, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert::(Ord a) => a -> (Tree a) -> (Tree a)
insert x EmptyTree = singleton x
insert x (Node a left right)
      | x == a = Node x left right
      | x > a =  Node a left (insert x right)
      | x < a = Node a (insert x left) right

empty::(Tree a) -> Bool
empty EmptyTree = True
empty (Node a left right) = False

isBinary::(Tree a) -> Bool
isBinary EmptyTree = True
isBinary (Node a left right) = True

search::(Ord a) => a -> (Tree a) -> Bool
search x EmptyTree = False
search x (Node a left right)
       | x == a = True
       | x > a = search x right
       | x < a = search x left

nnodes::(Tree a) -> Int
nnodes EmptyTree = 0
nnodes (Node a left right) = 1 + (nnodes left) + (nnodes right)

isBalanced::(Tree a) -> Bool
isBalanced EmptyTree = True
isBalanced (Node a left right) = (abs ((nnodes left) - (nnodes right))) <= 1

traverseTree::String -> (Tree a) -> [a]
traverseTree x EmptyTree = []
traverseTree x (Node a left right)
          | x == "LVR" = (traverseTree x left) ++ [a] ++ (traverseTree x right)
          | x == "VLR" = [a] ++ (traverseTree x left) ++ (traverseTree x right)
          | x == "LRV" = (traverseTree x left) ++ (traverseTree x right) ++ [a]
          | x == "VRL" = [a] ++ (traverseTree x right) ++ (traverseTree x left)
          | x == "RVL" = (traverseTree x right) ++ [a] ++ (traverseTree x left)
          | x == "RLV" = (traverseTree x right) ++ (traverseTree x left) ++ [a]
          | otherwise = error "Invalid String Argument"

toString::(Show a) => (Tree a) -> String
toString EmptyTree = ""
toString (Node a left right) = (show a) ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"

leaves::(Tree a) -> [a]
leaves x = traverseTree "LVR" x

nsum::(Num a) => (Tree a) -> a
nsum EmptyTree = 0
nsum (Node a left right) = a + (nsum left) + (nsum right)

tmap::(a -> b) -> (Tree a) -> (Tree b)
tmap x EmptyTree = EmptyTree
tmap x (Node a left right) = Node (x a) (tmap x left) (tmap x right)

findMin::(Ord a) => (Tree a) -> a
findMin (Node a left right)
    | left == EmptyTree = a
    | otherwise = findMin left

remove::(Ord a) => a -> (Tree a) -> (Tree a)
remove x EmptyTree = EmptyTree
remove x (Node a left right)
        | x < a = Node a (remove x left) right
        | x > a = Node a left (remove x right)
        | x == a && left == EmptyTree && right == EmptyTree = EmptyTree
        | x == a && left == EmptyTree = right
        | x == a && right == EmptyTree = left
        | x == a = Node (findMin right) left (remove (findMin right) right)

{-
merge::(Ord a) => (Tree a) -> (Tree a) -> (Tree a)
merge EmptyTree EmptyTree = EmptyTree
merge (Node a left right) EmptyTree = Node a left right
merge EmptyTree (Node a left right) = Node a left right
-}