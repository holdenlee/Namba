module Tree where

type Tree a = Leaf a | Node (List (Tree a))

type LTree a = Node a (List (Tree a))

{-| Get the left child of a tree-}
lchild : Tree a -> Tree a
lchild t = case t of
             Node [l,r] -> l
             _ -> t 
                 -- fail (it is a leaf)

{-| Get the right child of a tree-}
rchild : Tree a -> Tree a
rchild t = case t of
             Node [l,r] -> r
             _ -> t

{-| Combine the children of two trees. If one tree (or both) is a leaf, make it into a child.-}
combineTrees : Tree a -> Tree a -> Tree a
combineTrees t1 t2 = 
    case (t1,t2) of
      (Leaf x, Leaf y) -> Node [Leaf x, Leaf y]
      (Leaf x, Node li2) -> Node ((Leaf x)::li2)
      (Node li1, Leaf y) -> Node (li1++[Leaf y])
      (Node li1, Node li2) -> Node (li1++li2)
