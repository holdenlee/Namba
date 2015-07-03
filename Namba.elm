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

type BasicType = TInt | Var Int

type alias Type = Tree BasicType

opDict : D.Dict String (Int -> Int -> Int)
opDict = D.fromList [("+",(+)),("-",(-)),("*",(*)),("/",(//)),("%",(%))]

type Atom = AInt Int | AOp String | Compose

type alias Expr = Tree Atom

type alias PMatch = D.Dict Int Type

{-
Bomb deletes one block, clear deletes whole column.
here t is the template
-}
type Block = BExpr Expr Type | Copy | Bomb | Clear | Rock

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
--(\(t',t1') mpm -> (fmap (\x -> applyType x t' t1')) mpm) (Just pm) (map2 (,) li li1)    
                                         -- ex. TInt

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

lchild : Tree a -> Tree a
lchild t = case t of
             Node [l,r] -> l
             _ -> t 
-- fail condition

rchild : Tree a -> Tree a
rchild t = case t of
             Node [l,r] -> r
             _ -> t

combineTrees : Tree a -> Tree a -> Tree a
combineTrees t1 t2 = 
    case (t1,t2) of
      (Leaf x, Leaf y) -> Node [Leaf x, Leaf y]
      (Leaf x, Node li2) -> Node ((Leaf x)::li2)
      (Node li1, Leaf y) -> Node (li1++[Leaf y])
      (Node li1, Node li2) -> Node (li1++li2)

--first in list is on top
--BExpr Expr Type | Copy | Bomb | Clear | Rock | Empty
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
--eval being called wrongly here
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
                   --spawnProb : Float,
                   inputs : List Block,
                   nextInput : Seed -> (Block, Seed),
                   --generate a block and block loation regularly
                   nextInts : List Int,
                   spawnTime : Time,
                   spawnInts : Seed -> (List Int, Seed), 
                   score : Int,
                   timeSinceSpawn : Time
                  }

type Model = Waiting | Ingame Game

defWidth = 4

chooseByProbs : Float -> List (a,Float) -> a 
chooseByProbs r li = 
    case li of
      [] -> crash "invalid prob list"
      [(i,c)] -> i
      (i,c)::rest ->
          if r <= c 
          then i
          else chooseByProbs (r - c) rest

probListToInputFunc : List (Block, Float) -> Seed -> (Block, Seed)
probListToInputFunc li s =
    let
        (r, s') = generate (float 0 1) s
    in
      (chooseByProbs r li,s')

numBlock : Int -> Block
numBlock n = BExpr (Leaf (AInt n)) (Leaf TInt)

opBlock : String -> Block
opBlock str = BExpr (Leaf (AOp str)) (Node [Leaf TInt, Node [Leaf TInt, Leaf TInt]])

normalizeList : List (Block, Int) -> List (Block, Float)
normalizeList li = 
    let
        s = toFloat <| L.sum <| L.map (\(_,y) -> y) li
    in
      L.map (\(x,y) -> (x,(toFloat y)/s)) li

defInputFunc : Seed -> (Block, Seed)
defInputFunc = probListToInputFunc <| normalizeList 
               ((L.map (\x -> (numBlock x,1)) [(-10)..10])
                ++ [(Bomb, 1),
                    (Clear, 1),
                    (Copy, 2),
                    (Rock, 1)
                    ])

defGetInts : Int -> Seed -> (List Int, Seed)
defGetInts l s = generate (list l (int (-100) 100)) s


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
--|> A.set 0 [BExpr (Leaf (AInt 0)) (Leaf TInt)],
             x=0,
             curBlock = Nothing,
             activated = False,
             seed = s', 
                 --add a random seed
             --spawnProb = 0.025, 
             inputs = li,
             nextInput = defInputFunc,
             nextInts = li',
             spawnTime = 10*second,
             spawnInts = defGetInts 1,
                 --generate a block and block loation regularly
             score =0,
             timeSinceSpawn = 0
                }

curStack : Game -> List Block
curStack m = M.withDefault [] <| A.get m.x m.stacks

tail' : List a -> List a
tail' li = 
    case li of
      [] -> []
      h::rest -> rest

updateCurStack : (List Block -> List Block) -> Game -> Game
updateCurStack f m = 
    {m | stacks <- A.set m.x (f <| curStack m) m.stacks}
--dunno if this memo'izes curStack?

isGameOver : Game -> Bool
isGameOver g = L.length g.nextInts > g.h

step : Input -> Model -> Model
step inp m = 
    case m of 
      Ingame g -> 
          let 
              g' = stepGame inp g
          in
            if isGameOver g' then Waiting else Ingame g'
      Waiting -> 
          case inp of
            MoreBlocks i -> Ingame (startGame <| round i)
            _ -> Waiting

start = Waiting

spellDict = D.fromList [(1, opBlock "+"), (2, opBlock "-"), (3, opBlock "*"), (4, opBlock "/")]

--UPDATE
stepGame : Input -> Game -> Game
stepGame inp m = 
  --(watchSummary "stacks" (.stacks << getModel)) <| 
  watch "model" <| 
    case inp of
      Activate -> 
          case m.curBlock of
            Nothing -> m
            _ -> {m | activated <- True}
                 --if no current block, do not activate!
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
      Spell i -> 
          case m.curBlock of
            Nothing -> {m | curBlock <- D.get i spellDict}
            Just _ -> m                
      Drop -> 
          case m.curBlock of
            Nothing ->  m 
                       --no block to drop!
            Just b -> 
                let
                    cs = curStack ( m)
                in
                  if L.length cs < m.h
                  then
                       {m | curBlock <- Nothing,
                                 activated <- False} 
                         |> updateCurStack (if m.activated then applyStack b else (\x -> b::x))
                         |> resolveSuccesses
                  else 
                       m
      TimeDelta td ->
          let
              newT = m.timeSinceSpawn + td
          in
            --assume there isn't so much lag that two time intervals pass between updates
            if newT > m.spawnTime 
            then 
                let
                    (li, s') = m.spawnInts m.seed
                in
                   {m | seed <- s',
                        timeSinceSpawn <- newT - m.spawnTime,
                        nextInts <- m.nextInts ++ li,
                        spawnTime <- 100*second/((toFloat m.score) + 10)
                        }
            else  {m | timeSinceSpawn <- newT}
      MoreBlocks _ -> 
          case m.inputs of
            [] ->  m 
                  --shouldn't happen
            h::rest -> 
               if L.length (curStack ( m)) < m.h
               then
                   let
                       (b, s) = m.nextInput m.seed
                   in
                      {m | seed <- s,
                                inputs <- rest ++ [b]} 
                       |> updateCurStack (applyStack h)
                       |> resolveSuccesses
               else 
                    m

resolveSuccesses' :  Game -> Game 
resolveSuccesses' m = 
    case L.foldl (resolveSuccess) m [0..(m.w-1)] of
       m2 ->
          if L.length m2.nextInts == L.length m.nextInts 
          then m2
          else resolveSuccesses' m2
--this does it all in one go. (However, may want to change so does only 1 at a time, for sake of visual effects.)
--TODO: add in new

--being lazy right now - this is not what I want
resolveSuccesses :  Game -> Game 
resolveSuccesses = resolveSuccesses'                    

{- index -}
resolveSuccess : Int -> Game -> Game 
resolveSuccess n m = 
    case (A.get m.x m.stacks) of
      Just ((BExpr (Leaf (AInt p)) _)::rest) -> 
          --clunky...
          case m.nextInts of
            [] ->  m
                  --no more blocks left to match
            h::following -> 
               if p == h 
               then 
                   {m | stacks <- A.set m.x rest m.stacks,
                        nextInts <- following,
                        score <- m.score + 1
                                  --need to add in new!
                                  --delete (SHOULD THIS BE THE BEHAVIOR?)
                   }
               else 
                   m
      _ -> m

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

--given the width w, height h, (x,y) where the elements should be situated, the elements, creates an element combining all of these.
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
            --Text.fromString showExpr ex
                   _ -> show b]


renderBlock : Block -> Element
renderBlock = renderBlock' bwidth bheight

--#blocks high
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
renderGame m = 
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
    collage w h [(Text.fromString "Press SPACE to begin." |> Text.color white) |> centered |> toForm] |> color black


--SIGNAL
type Input = Activate | Drop | Pickup | Left | Right | MoreBlocks Time | TimeDelta Time | Spell Int

whenPress : Signal Bool -> Signal Bool
whenPress k = filter identity False <| dropRepeats k

keyCodeToState : Int -> a -> Signal a 
keyCodeToState k state = map (always state) (whenPress (isDown k))

spell : Signal Input
spell = mergeMany (L.map (\i -> keyCodeToState (48+i) (Spell i)) [1..4])

{- 48+ -}
{-
   38
37 40 39
-}
input : Signal Input 
input = mergeMany [keyCodeToState 65 Activate, 
                   keyCodeToState 40 Drop,
                   keyCodeToState 38 Pickup,
                   keyCodeToState 37 Left,
                   keyCodeToState 39 Right,
                   map (\(i,_) -> MoreBlocks i) (timestamp <| whenPress (isDown 32)),
--^this is hacky right now!
--                   keyCodeToState 32 MoreBlocks,
--http://stackoverflow.com/questions/29453679/how-do-i-get-the-current-time-in-elm
                                  --space
                   (map TimeDelta (fps 20)),
                   spell
                  ]

main = map render (foldp step start input)
