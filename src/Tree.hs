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