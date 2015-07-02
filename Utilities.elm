module Utilities (..) where

import List exposing (..)
import Dict as D
import Maybe as M
import Array as A

--Lists (Note: array is better at these things)

(!): List a -> Int -> Maybe a
li ! i = head (drop i li)

modifyAt: Int -> List a -> List a -> List a
modifyAt i li2 li = (take i li) ++ li2 ++ (drop (i+1) li)

modify:Int -> a -> List a -> List a
modify i x li = modifyAt i [x] li

getFirstNonzero : List Int -> Int
getFirstNonzero li = case li of
                       [] -> 0
                       x::rest -> if x/=0 then x else getFirstNonzero rest

listprod: List a -> List b -> List (a,b)
listprod l1 l2 = concat 
                 (map (\x -> 
                           (map (\y -> (x,y)) l2))
                  l1)

--array

getWithDefault: a -> Int -> A.Array a -> a
getWithDefault x i ar = M.withDefault x (A.get i ar)

--inclusive
range: Int -> Int -> List Int
range x y = if (x>y) then [] else x::(range (x+1) y)


--(!!): List Int -> Int -> Int
--li !! i = if (i<length li) then (li!i) else 0

--tuples 
smult:Int -> (Int,Int)-> (Int,Int)
smult c (x,y) = (c*x,c*y)

--mapping

zip : List a -> List b -> List (a,b)
zip l1 l2 = map2 (,) l1 l2

zipMap : (a -> b -> c) -> List a -> List b -> List c
zipMap f al bl = map (\(x,y) -> f x y) (zip al bl)

--folding
foldlRot: (a -> b -> b) -> List a -> b -> b
foldlRot f x y = foldl f y x

while: (b -> Bool) -> b -> (b -> b) -> b
while f x0 update = if (f x0) 
                    then (while f (update x0) update) 
                    else x0

--getting from dicts and lists
--def is default
getD:comparable -> a -> D.Dict comparable a -> a
getD str def d = M.withDefault def (D.get str d)

getL: List a -> Int -> a -> a
getL li i def = M.withDefault def (li!i)
--if (i<length li) then (li!i) else def

--Maybe
maybe: b -> (a -> b) -> Maybe a -> b
maybe def f mx = case mx of
                   Nothing -> def
                   Just x -> f x
