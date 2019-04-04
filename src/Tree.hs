module Tree where

data Tree a = 
        Node {
            leftChild::Tree a,
            rightChild:: Tree a,
            value::A}
        |Empty 