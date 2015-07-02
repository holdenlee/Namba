module Tree where

type Tree a = Leaf a | Node (List (Tree a))