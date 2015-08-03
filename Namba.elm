import Color exposing (..)
import Keyboard exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Array as A
import List as L
import Dict as D
import Maybe as M
import Graphics.Input exposing (..)
import Random exposing (..)
import Debug exposing (..)
import Text
import Mouse as Mo

import Tree exposing (..)
import Utilities exposing (..)

--Typing system

type BasicType = TInt | Var Int

type alias Type = Tree BasicType

type Atom = AInt Int | AOp String | Compose

type alias Expr = Tree Atom

type alias PMatch = D.Dict Int Type

{-| Clear deletes block.
-}
type Block = BExpr Expr Type | Copy | Clear

{-| Example: t is (Var 0 -> Var 0) and t1 is (Int -> Int). Then applyType will match "Var 0" with Int and return the dictionary mapping "Var 0" to Int.
pm is the existing mappings.
-}
applyType : PMatch -> Type -> Type -> Maybe PMatch
applyType pm t t1 =
    case t of
      Leaf a -> 
          case a of 
            Var n -> 
                case D.get n pm of
                  Just t2 ->
                      if t1 == t2 then Just pm else Nothing
                  Nothing ->
                      Just (pm |> D.insert n t1)
            _ -> if t==t1 then Just pm else Nothing
      Node li ->
          case t1 of
            Leaf _ -> Nothing
            Node li1 -> (L.foldl (flip M.andThen) (Just pm) <| L.map (\(t',t1') -> (\x -> applyType x t' t1')) (L.map2 (,) li li1))

{-| Substitute the variables in t by looking up in pm. -}
subst : PMatch -> Type -> Type
subst pm t = 
    case t of
      Leaf a -> 
          case a of
            Var n -> 
                case D.get n pm of
                  Just t2 -> t2
                  _ -> t
            _ -> t
      Node li -> Node (L.map (subst pm) li)

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

{-| Apply the block b to the block b1 and obtain another block. This is called when block b is put on top of b1. -}
apply : Block -> Block -> Maybe (Maybe Block, Maybe Block)
apply b b1 = 
    case b |> watch "1" of 
      BExpr ex t  ->
          case b1 |> watch "2" of 
            BExpr ex1 t1 ->
                case (applyType D.empty (lchild t) t1) |> watch "3" of
                  Just pm -> 
                      case (subst pm t) |> watch "4" of
                        Node [l, r] -> 
                            let
                                t2 = r
                                    --rchild t
                                ex2 = (combineTrees ex ex1)
                            in
                              if (t2 |> watch "5") == Leaf TInt
                              then Just (Just (BExpr (Leaf <| AInt <| eval ex2) t2), Nothing)
                              else Just (Just (BExpr (combineTrees ex ex1) (rchild t)), Nothing)
                        _ -> Nothing
                             --fail
                  Nothing -> Nothing
                       --wrong type!
            _ -> Nothing
      Copy -> Just (Just b1, Just b1)
      Clear -> Just (Nothing, Nothing)

type PlaceCode = Null | Placed | PickedUp | PlacedAndPickedUp

--should make an invalid click clear current block
{-| Apply block b to the Maybe block bs. Output is (placed block, picked-up block, exit code. -}
clickHole : Maybe Block -> Maybe Block -> (Maybe Block, Maybe Block, PlaceCode)
clickHole b b2 = 
    case b of
      Nothing ->
          (b2, b2, PickedUp)
          --no block in hand, so pick up block
      Just b' ->
          case b2 of 
            Nothing -> (b, Nothing, Placed)
            --placed at an empty hole
            Just b2' -> 
                 case apply b' b2' of
                   Nothing -> (b2, b2, PickedUp)
                   --failed to place, so pick up b2 instead
                   Just (c, c2) -> 
                       (c, c2, 
                         case c2 of
                           Just _ -> Placed 
                           --copy
                           Nothing -> PlacedAndPickedUp)

--MODEL
type alias Game = {w: Int,
                   h: Int,
                   intH : Int,
                   isPopUp : Int -> Int -> Bool,
                   palette : A.Array (A.Array (Maybe Block)),
                   prevLoc: Maybe (Int, Int),
                   curBlock : Maybe Block,
                   activated : Bool,
                   seed: Seed,
                   nextInput : Seed -> (Block, Seed),
                   nextInts : List Int,
                   spawnTime : Time,
                   spawnInts : Seed -> (List Int, Seed), 
                   score : Int,
                   timeSinceSpawn : Time,
                   gameOver : Bool
                  }

type Model = Waiting | Ingame Game

defWidth = 4
intH = 10

{-| Given a random number in [0,1], and a list of results with corresponding probabilities, choose a result. For example, 
    chooseByProbs x [(1,0.3), (2,0.7)]
chooses 1 if 0<=x<=0.3, and 2 if 0.3<x<=1.
-}
chooseByProbs : Float -> List (a,Float) -> a 
chooseByProbs r li = 
    case li of
      [] -> crash "invalid prob list"
      [(i,c)] -> i
      (i,c)::rest ->
          if r <= c 
          then i
          else chooseByProbs (r - c) rest

{-| Given a list of blocks with corresponding probabilities, create a function which gives the next block.-}
probListToInputFunc : List (Block, Float) -> Seed -> (Block, Seed)
probListToInputFunc li s =
    let
        (r, s') = generate (float 0 1) s
    in
      (chooseByProbs r li,s')

{-| Creates a number block.-}
numBlock : Int -> Block
numBlock n = BExpr (Leaf (AInt n)) (Leaf TInt)

{-| Creates a operation block.-}
opBlock : String -> Block
opBlock str = BExpr (Leaf (AOp str)) (Node [Leaf TInt, Node [Leaf TInt, Leaf TInt]])

{-| Dictionary of operations.-}
opDict : D.Dict String (Int -> Int -> Int)
opDict = D.fromList [("+",(+)),("-",(-)),("*",(*)),("/",(//)),("%",(%))]

{-| Turns numbers into probabilities.-}
normalizeList : List (Block, Int) -> List (Block, Float)
normalizeList li = 
    let
        s = toFloat <| L.sum <| L.map (\(_,y) -> y) li
    in
      L.map (\(x,y) -> (x,(toFloat y)/s)) li

{-| Default function giving the next input.-}
defInputFunc : Seed -> (Block, Seed)
defInputFunc = probListToInputFunc <| normalizeList 
               ((L.map (\x -> (numBlock x,2)) ([1..6]++[10]))++
               (L.map (\x -> (numBlock x,1)) ([(-10)..0]++[7..9]))++
               [(Clear, 1), (Copy, 2)])
{-
               ((L.map (\x -> (numBlock x,1)) [(-10)..10])
                ++ [(Copy, 1)
                    ])-}
--(Clear, 1),

{-| Default function giving the next integer to make.-}
defGetInts : Int -> Seed -> (List Int, Seed)
defGetInts l s = generate (list l (int (-100) 100)) s

set2 : (Int, Int) -> a -> A.Array (A.Array a) -> A.Array (A.Array a)
set2 (i,j) it arr = A.set i (getA i arr |> A.set j it) arr

fillIn : (Int, Int) -> Game -> Game
fillIn (i,j) g = 
    case g.palette |> getA i |> getA j of
      Nothing -> 
          if g.isPopUp i j 
          then
              let
                  (b, s') = g.nextInput g.seed 
              in
                {g | palette <- set2 (i,j) (Just b) g.palette, 
                     seed <- s'}          
          else g
      Just _ -> g

{-| Start game with given seed.-}
startGame : Int -> Game
startGame n = 
    let 
        h=4
        (li,s) = defGetInts 5 (initialSeed n)
        g = {w=defWidth,
             h=h,
             intH = intH,
             isPopUp = (\i j -> j < 2),
             palette = A.repeat h (A.repeat defWidth Nothing),
             prevLoc = Nothing,
             curBlock = Nothing,
             activated = False,
             seed = s,
             nextInput = defInputFunc,
             nextInts = li,
             spawnTime = 20*second,
             spawnInts = defGetInts 1,
             score =0,
             timeSinceSpawn = 0,
             gameOver = False
                }
    in
      for [0..(h-1)] g (\i g0 ->
         for [0..(g.w-1)] g0 (\j g1 ->
            fillIn (i,j) g1))

tail' : List a -> List a
tail' li = 
    case li of
      [] -> []
      h::rest -> rest

{-| Game is over if stack overflows.-}
isGameOver : Game -> Bool
isGameOver g = L.length g.nextInts > g.intH

--UPDATE
step : Input -> Model -> Model
step inp m = 
    case m of 
      Ingame g -> 
          if g.gameOver
          then
              case inp of
                StartGame _ -> Waiting
                _ -> Ingame g
          else
              let 
                  g' = stepGame inp g
              in
                if isGameOver g' then Ingame {g' | gameOver <- True} else Ingame g'
      Waiting -> 
          case inp of
            StartGame i -> Ingame (startGame <| round i)
            _ -> Waiting

start = Waiting

spellDict = D.fromList [(1, opBlock "+"), (2, opBlock "-"), (3, opBlock "*"), (4, opBlock "/")]

stepGame : Input -> Game -> Game
stepGame inp g = 
  --(watchSummary "stacks" (.stacks << getModel)) <| 
  watch "model" <| 
    case inp of
      Activate b ->
          {g | activated <- b}
      Spell i -> 
          --look up the ith operation and apply it to the stack.
          let
              b = D.get i spellDict
          in
            {g | curBlock <- b, prevLoc <- Nothing}
      Click (x,y) ->
          let
              (i,j) = getLocFromMouse (x,y) g
              (b, b2, code) = clickHole g.curBlock (g.palette |> getA i |> getA j)
          in
            {g | palette <- set2 (i,j) b g.palette,
                 curBlock <- b2} |> (case code of 
                                       Null -> (\g0 -> {g0 | prevLoc <- Nothing})
                                       Placed -> 
                                           case g.prevLoc of
                                             Just (i',j') -> 
                                                 (\g0 -> {g0 | palette <- set2 (i',j') Nothing g0.palette, prevLoc <- Nothing} |> fillIn (i',j') |> fillIn (i,j))
                                             Nothing -> fillIn (i,j)
                                       PickedUp -> (\g0 -> {g0 | prevLoc <- Just (i,j)})
                                       PlacedAndPickedUp -> 
                                           (case g.prevLoc of
                                             Just (i',j') -> 
                                                 (\g0 -> {g0 | palette <- set2 (i',j') Nothing g0.palette, prevLoc <- Nothing} |> fillIn (i',j') |> fillIn (i,j))
                                             Nothing -> fillIn (i,j)) >> (\g0 -> {g0 | curBlock <- b, prevLoc <- Just (i,j)})) |> resolveSuccesses
--should make b2 equal to b here??
--bug when stack is empty?
      TimeDelta td ->
          let
              newT = g.timeSinceSpawn + td
          in
            --assume there isn't so much lag that two time intervals pass between updates
            if newT > g.spawnTime 
            then 
                --add a new block to the right-hand stack (using the spawnInts function), and reset timeSinceSpawn.
                let
                    (li, s') = g.spawnInts g.seed
                in
                   {g | seed <- s',
                        timeSinceSpawn <- newT - g.spawnTime,
                        nextInts <- g.nextInts ++ li,
                        spawnTime <- 200*second/((toFloat g.score) + 10)
                        }
            else  {g | timeSinceSpawn <- newT}

{-| Check for blocks that match the integers to be made. Remove them and increment score.-}
resolveSuccesses :  Game -> Game 
resolveSuccesses g = 
    let
        g' = for [0..(g.h-1)] g (\i g0 ->
                for [0..(g.w-1)] g0 (\j g1 -> 
                   resolveSuccess (i,j) g1))
    in
      if g.score == g'.score 
      then g
      else resolveSuccesses g'
              
{-| Check for matching block just in the nth stack.-}
resolveSuccess : (Int, Int) -> Game -> Game 
resolveSuccess (i,j) m = 
    case (m.palette |> getA i |> getA j) of
      Just (BExpr (Leaf (AInt p)) _) -> 
          --clunky...
          case m.nextInts of
            [] -> m
                  --no more blocks left to match
            h::following -> 
               if p == h 
               then {m | palette <- set2 (i,j) Nothing m.palette,
                         nextInts <- following,
                         score <- m.score + 1
                             --delete (SHOULD THIS BE THE BEHAVIOR?)
                    } |> fillIn (i,j)
               else m
      _ -> m

{-| Evaluate the expression to give an integer. The type corresponding to `ex` must be Int!-}
eval : Expr -> Int
eval ex = 
    case ex of
      Leaf (AInt n) -> n
      Node (h::rest) ->
          case h of
            Leaf (AInt n) -> n
            Leaf (AOp str) -> 
                case rest of 
                  [p,q] -> (M.withDefault (+) <| D.get str opDict) (eval p) (eval q)
                  _ -> 9991 
                       --fail
            Leaf Compose -> 
                case rest of
                  [f,g,x] -> eval (Node [f,Node [g,x]])
                  _ -> 9992
                       --fail
            Node li -> eval (Node (li ++ rest))
      _ -> 9993
           --fail

intersperse: String -> List String -> String
intersperse str li = 
    case li of
      [] -> ""
      [x] -> x
      h::rest -> h ++ str ++ (intersperse str rest)

{-| Display expression (appears on the block).-}
showExpr : Expr -> String
showExpr ex = 
    case ex of 
      Leaf (AInt n) -> toString n
      Leaf (AOp s) -> s
      Leaf x -> toString x
      Node li -> (\x -> "(" ++ x ++ ")") <| intersperse " " <| L.map showExpr li

bwidth = 150
bheight = 150
bheight2 = 40

--VIEW

{-| given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.-}
collageAbs : Int -> Int -> List (Int,Int) -> List Element -> Element
collageAbs w h tuples elems = 
    collage w h (zipMap (\(x,y) -> \elem -> move ( (-(toFloat w)/2 + (toFloat (widthOf elem))/2 + toFloat x),  (-(toFloat h)/2 + (toFloat (heightOf elem))/2 + toFloat y)) (toForm elem)) tuples elems)

renderBlock' : Int -> Int -> Block -> Element
renderBlock' w h b = 
--outlined defaultLine
    layers [let 
               c = 
                   case b of
                     BExpr (Leaf (AInt _)) _ -> orange
                     BExpr (Leaf (AOp _)) _ -> yellow
                     BExpr (Node _) _ -> green
                     _ -> blue
            in
              collage w h <| (\x -> [x]) <| filled c <| rect (toFloat w) (toFloat h),
            container w h middle <|
                 case b of
                   BExpr ex _ -> centered (Text.height 30 <| Text.monospace (Text.fromString (showExpr ex)))
                   _ -> centered (Text.height 20 <| Text.monospace (Text.fromString (toString b)))]

renderBlock : Block -> Element
renderBlock = renderBlock' bwidth bheight

renderMBlock : Maybe Block -> Element
renderMBlock mb = 
    case mb of 
      Just b -> renderBlock b 
      Nothing -> container bwidth bheight middle empty

renderRow : Int -> A.Array (Maybe Block) -> Element
renderRow w li = container (w*bwidth) bheight midLeft <| flow right (L.map renderMBlock <| A.toList li)

--h is the number of blocks high
{-renderStack : Int -> List Block -> Element 
renderStack h li = container bwidth (h*bheight) midBottom <| flow down (L.map renderBlock li)-}

renderPalette : Int -> A.Array (A.Array (Maybe Block)) -> Element
renderPalette n = (flow down) << (L.map (renderRow n)) << A.toList

column w h = container w h midBottom

--bheight2*intH
renderNextInts : Int -> List Int -> Element
renderNextInts h li = column 60 (bheight*h) <| flow down <| L.map (renderBlock' 60 bheight2 << numBlock) li

renderInfoPanel : Game -> Element
renderInfoPanel m = container bwidth (bheight*(m.h)) middle <| flow down [centered <| (Text.height 30 <| Text.monospace (Text.fromString (toString m.score))), renderMBlock m.curBlock]

renderGame : Game -> Element
renderGame g = 
    layers [renderGame' g, if g.gameOver
                           then centered <| Text.height 30 <| Text.monospace (Text.fromString "Stack Overflow! Press ENTER to continue.")
                           else empty]
    

renderGame' : Game -> Element
renderGame' m = 
    flow right
         [renderPalette m.h m.palette,
          renderNextInts m.h m.nextInts,
          renderInfoPanel m]

totalW = 4 * bwidth + 60 + 150
totalH = 4 * bheight

render : Model -> Element
render m = 
    case m of
      Ingame g -> renderGame g
      Waiting -> renderWaiting totalW totalH

renderWaiting : Int -> Int -> Element
renderWaiting w h = 
    let
        rw str = (Text.fromString str |> Text.color white) |> centered
    in
      (container w h middle <| flow down <| L.map rw ["Press ENTER to start.",
                                                      "LEFT/RIGHT: Move Pete.",
                                                      "UP: Pick up block.",
                                                      "DOWN: Drop block.",
                                                      "SPACE: Drop block from input stack.",
                                                      "1,2,3,4: Drop +, -, *, /.",
                                                      "SHIFT (+DOWN, SPACE, 1, 2, 3, 4): Drop and activate block."]) |> color black

--SIGNAL
type Input = Click (Int, Int) | Activate Bool | BlockFromInput | StartGame Time | TimeDelta Time | Spell Int

click : Signal Input
click = map Click <| sampleOn Mo.clicks Mo.position

getLocFromMouse : (Int, Int) -> Game -> (Int, Int)
getLocFromMouse (x,y) g = (clamp 0 (g.h-1) (y // bheight), clamp 0 (g.w-1) (x // bwidth))

whenPress : Signal Bool -> Signal Bool
whenPress k = filter identity False <| dropRepeats k

keyCodeToState : Int -> a -> Signal a 
keyCodeToState k state = map (always state) (whenPress (isDown k))

{-| Convert a digit press to the signal of the corresponding "spell". Digit i is 48+i. -}
spell : Signal Input
spell = mergeMany (L.map (\i -> keyCodeToState (48+i) (Spell i)) [1..4])

{-
   38
37 40 39
-}
input : Signal Input 
input = mergeMany [click,
                   map Activate shift,
                   keyCodeToState 32 BlockFromInput,
                   --space
                   map (\(i,_) -> StartGame i) (timestamp <| whenPress (isDown 13)),
                   --enter
                   --http://stackoverflow.com/questions/29453679/how-do-i-get-the-current-time-in-elm
                   map TimeDelta (fps 20),
                   spell
                  ]

main = map render (foldp step start input)
