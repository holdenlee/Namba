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

import Tree exposing (..)
import Utilities exposing (..)

--Typing system

type BasicType = TInt | Var Int

type alias Type = Tree BasicType

type Atom = AInt Int | AOp String | Compose

type alias Expr = Tree Atom

type alias PMatch = D.Dict Int Type

{-| Bomb deletes one block, Clear deletes whole column.
Currently, only BExpr, Copy, and Clear are used.
-}
type Block = BExpr Expr Type | Copy | Bomb | Clear | Rock

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

{-| Apply the block b to the block b1 and obtain a list of blocks. This is called when block b is put on top of b1. -}
apply : Block -> Block -> List Block
apply b b1 = 
    case b |> watch "1" of 
      BExpr ex t  ->
          case b1 |> watch "2" of 
            BExpr ex1 t1 ->
                case (applyType D.empty (lchild t) t1) |> watch "3" of
                  Just pm -> 
                      case (subst pm t) |> watch "4"of
                        Node [l, r] -> 
                            let
                                t2 = r
                                    --rchild t
                                ex2 = (combineTrees ex ex1)
                            in
                              if (t2 |> watch "5") == Leaf TInt
                              then [BExpr (Leaf <| AInt <| eval ex2) t2]
                              else [BExpr (combineTrees ex ex1) (rchild t)]
                        _ -> [b,b1]
                             --fail
                  Nothing -> [b,b1]
                       --wrong type!
            _ -> [b, b1]
      Copy -> [b1,b1]
      Bomb -> []
      Clear -> [Clear]
      Rock -> [Rock, b1]

{-| Apply block b to the stack (list of blocks) bs. Basically this is repeated application of `apply`.-}
applyStack : Block -> List Block -> List Block
applyStack b bs = 
    case bs of
      [] ->
          case b of
            Copy -> []
            Bomb -> []
            Clear -> []
            _ -> [b]
      h::rest ->
          let
              bs2 = apply b h
          in
            case bs2 of
              [b2] -> applyStack b2 rest
              _ -> bs2 ++ rest


--MODEL
type alias Game = {w: Int,
                   h: Int,
                   stacks : A.Array (List Block),
                   x: Int,
                   curBlock : Maybe Block,
                   activated: Bool,
                   seed: Seed,
                   inputs : List Block,
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
               ((L.map (\x -> (numBlock x,1)) [(-10)..10])
                ++ [(Clear, 1),
                    (Copy, 3)
                    ])

{-| Default function giving the next integer to make.-}
defGetInts : Int -> Seed -> (List Int, Seed)
defGetInts l s = generate (list l (int (-100) 100)) s

{-| Start game with given seed.-}
startGame : Int -> Game
startGame n = 
    let 
        h=10
        (li,s) = generate (list h (customGenerator defInputFunc)) (initialSeed n)
        (li',s') = defGetInts 5 s
    in
            {w=defWidth,
             h=h,
             stacks=A.repeat defWidth [], 
             x=0,
             curBlock = Nothing,
             activated = False,
             seed = s', 
             inputs = li,
             nextInput = defInputFunc,
             nextInts = li',
             spawnTime = 20*second,
             spawnInts = defGetInts 1,
             score =0,
             timeSinceSpawn = 0,
             gameOver = False
                }

{-| Get the stack that Pete is standing above.-}
curStack : Game -> List Block
curStack m = M.withDefault [] <| A.get m.x m.stacks

tail' : List a -> List a
tail' li = 
    case li of
      [] -> []
      h::rest -> rest

{-| Updates current stack with function `f`.-}
updateCurStack : (List Block -> List Block) -> Game -> Game
updateCurStack f m = 
    {m | stacks <- A.set m.x (f <| curStack m) m.stacks}

{-| Game is over if stack overflows.-}
isGameOver : Game -> Bool
isGameOver g = L.length g.nextInts > g.h

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
stepGame inp m = 
  --(watchSummary "stacks" (.stacks << getModel)) <| 
  watch "model" <| 
    case inp of
      Activate b -> 
          {m | activated <- b}
      Left ->
          {m | x <- (m.x-1)%(m.w)}
      Right -> 
          {m | x <- (m.x+1)%(m.w)}
      Pickup ->
          case (A.get m.x m.stacks, m.curBlock) of
            (Just [],_) ->  m
            (_, Just _) ->  m
                --if either the stack is empty or already have a block, do not pick up one.
            (Just (top::_), _) ->  {m | curBlock <- Just top}
                                         |> updateCurStack tail'
                                         |> resolveSuccesses
                --pick up a block.
      Spell i -> 
          --look up the ith operation and apply it to the stack.
          let
              b = M.withDefault (numBlock 0) <| D.get i spellDict
          in
            if L.length (curStack m) < m.h
            then m |> updateCurStack (if m.activated then applyStack b else (\x -> b::x))
                   |> resolveSuccesses
            else m      
      Drop -> 
          case m.curBlock of
            Nothing ->  m 
                --no block to drop!
            Just b -> 
                if L.length (curStack m) < m.h
                then {m | curBlock <- Nothing} 
                          |> updateCurStack (if m.activated then applyStack b else (\x -> b::x))
                          |> resolveSuccesses
                else m
      TimeDelta td ->
          let
              newT = m.timeSinceSpawn + td
          in
            --assume there isn't so much lag that two time intervals pass between updates
            if newT > m.spawnTime 
            then 
                --add a new block to the right-hand stack (using the spawnInts function), and reset timeSinceSpawn.
                let
                    (li, s') = m.spawnInts m.seed
                in
                   {m | seed <- s',
                        timeSinceSpawn <- newT - m.spawnTime,
                        nextInts <- m.nextInts ++ li,
                        spawnTime <- 200*second/((toFloat m.score) + 10)
                        }
            else  {m | timeSinceSpawn <- newT}
      BlockFromInput -> 
          --drop a block from input stack.
          case m.inputs of
            [] ->  m 
                  --shouldn't happen
            h::rest -> 
               if L.length (curStack m) < m.h
               then
                   let
                       (b, s) = m.nextInput m.seed
                   in
                      {m | seed <- s,
                           inputs <- rest ++ [b]} 
                       |> updateCurStack (if m.activated then applyStack h else (\x -> h::x))
                       |> resolveSuccesses
               else 
                    m

{-| Check for blocks that match the integers to be made. Remove them and increment score.-}
resolveSuccesses :  Game -> Game 
resolveSuccesses m = 
    case L.foldl (resolveSuccess) m [0..(m.w-1)] of
       m2 ->
          if L.length m2.nextInts == L.length m.nextInts 
          then m2
          else resolveSuccesses m2
--this does it all in one go. (However, may want to change so does only 1 at a time, for sake of visual effects.)                
{-| Check for matching block just in the nth stack.-}
resolveSuccess : Int -> Game -> Game 
resolveSuccess n m = 
    case (A.get n m.stacks) of
      Just ((BExpr (Leaf (AInt p)) _)::rest) -> 
          --clunky...
          case m.nextInts of
            [] ->  m
                  --no more blocks left to match
            h::following -> 
               if p == h 
               then 
                   {m | stacks <- A.set n rest m.stacks,
                        nextInts <- following,
                        score <- m.score + 1
                            --delete (SHOULD THIS BE THE BEHAVIOR?)
                   }
               else 
                   m
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

bwidth = 200
bheight = 40

--VIEW

{-| given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.-}
collageAbs : Int -> Int -> List (Int,Int) -> List Element -> Element
collageAbs w h tuples elems = 
    collage w h (zipMap (\(x,y) -> \elem -> move ( (-(toFloat w)/2 + (toFloat (widthOf elem))/2 + toFloat x),  (-(toFloat h)/2 + (toFloat (heightOf elem))/2 + toFloat y)) (toForm elem)) tuples elems)

renderPete : Game -> Element
renderPete m = 
    case m.curBlock of
      Nothing -> container bwidth (bheight+30) midBottom <| image 30 30 "pete.gif"
      Just b -> (renderBlock b) `above` (container bwidth 30 middle <| image 30 30 "hold.gif")

renderTop : Game -> Element
renderTop m = flow right <| L.map (\i -> if i==m.x then renderPete ( m) else (container bwidth (bheight + 30) middle empty)) [0..(m.w-1)]

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

--h is the number of blocks high
renderStack : Int -> List Block -> Element 
renderStack h li = container bwidth (h*bheight) midBottom <| flow down (L.map renderBlock li)

renderStacks : Int -> A.Array (List Block) -> Element
renderStacks n = (flow right) << (L.map (renderStack n)) << A.toList

column w h = container w h midBottom

renderInputs : Int -> List Block -> Element
renderInputs h li = column 60 (bheight*(h+1) + 30) <| flow down <| L.map (renderBlock' 60 bheight) li

renderNextInts : Int -> List Int -> Element
renderNextInts h li = column 60 (bheight*(h+1) + 30) <| flow down <| L.map (renderBlock' 60 bheight << numBlock) li

renderInfoPanel : Game -> Element
renderInfoPanel ( m) = container 60 (bheight*(m.h+1) + 30) middle <| centered <| (Text.height 30 <| Text.monospace (Text.fromString (toString m.score)))

renderGame : Game -> Element
renderGame g = 
    layers [renderGame' g, if g.gameOver
                           then centered <| Text.height 30 <| Text.monospace (Text.fromString "Stack Overflow! Press ENTER to continue.")
                           else empty]
    

renderGame' : Game -> Element
renderGame' m = 
    flow right
         [renderInputs m.h m.inputs,
          renderTop m `above` renderStacks m.h m.stacks,
          renderNextInts m.h m.nextInts,
          renderInfoPanel m]

totalW = 4 * bwidth + 3*60
totalH = 11 * bheight + 30

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
type Input = Activate Bool | Drop | Pickup | Left | Right | BlockFromInput | StartGame Time | TimeDelta Time | Spell Int

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
input = mergeMany [map Activate shift,
                   --keyCodeToState 65 Activate, 
                   keyCodeToState 40 Drop,
                   --down
                   keyCodeToState 38 Pickup,
                   --up
                   keyCodeToState 37 Left,
                   keyCodeToState 39 Right,
                   keyCodeToState 32 BlockFromInput,
                   --space
                   map (\(i,_) -> StartGame i) (timestamp <| whenPress (isDown 13)),
                   --enter
                   --http://stackoverflow.com/questions/29453679/how-do-i-get-the-current-time-in-elm
                   map TimeDelta (fps 20),
                   spell
                  ]

main = map render (foldp step start input)
