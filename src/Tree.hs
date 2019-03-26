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

