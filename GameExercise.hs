{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module GameExercise where
import Data.List
{-

Before running your solution in ghci or compiling it by ghc on lab machines
make sure you run

    module load ghc/7.6.3

(This is to make sure your version of GHC supports Safe language extension.)

-}

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))
import System.Random

-- We have two players PV (the vertical player) and PH (the horizontal player).
-- More information is in the pdf handout on Canvas.

data Player = PV | PH deriving (Read, Show, Eq, Ord)

-- Define Outcome (will only use 1 or -1 as there's no draw)
type Outcome = Int

-- You need to define a type of boards:

data Board = Board {
                  player         :: Player -- Next player (PV or PH)
                 ,xBound         :: Int
                 ,yBound         :: Int
                 ,occupiedPosit  :: [(Int, Int)] -- List of occupied positions (Follows convention)
                 ,outcome        :: Outcome -- Either 1 or -1
                   } deriving (Read, Show)

-- You also need to provide a type of moves:

data Move = V Int Int | H Int Int deriving (Eq, Show)

-- You will work with the definition of Tree as in our tic-tac-toe
-- lectures and handouts:

data Tree = Fork {root :: Board, children :: [(Move,Tree)]} deriving (Show)

-- In order to test your program, we will need to create boards,
-- without knowing how you choose to represent boards. You have to
-- implement the following function toBoard for that purpose.
--
-- Input: a tuple (xBound, yBound, coordinates, player) where
--
--     (1) xBound and yBound are non-negative Int's
--     (2) coordinates is a list of pairs (x,y) with 0 <= x < xBound
--                                               and 0 <= y < yBound
--         which are to be occupied in the board.
--     (3) player is the Player that plays next in that board.
--
-- Output: a Board according to your choice of representation.

genHMoves :: Int -> Int -> [Move]
genHMoves 0 _ = []
genHMoves _ (-1) = []
genHMoves x y = [H n y | n <- [0..x-1]] ++ (genHMoves x (y-1))

genVMoves :: Int -> Int -> [Move]
genVMoves (-1) _ = []
genVMoves _ 0 = []
genVMoves x y = [V x n | n <- [0..y-1]] ++ (genVMoves (x-1) y)
            
allMoves :: Int -> Int -> [Move]
allMoves x y = genHMoves (x-1) (y-1) ++ genVMoves (x-1) (y-1)  

emptyBoard :: Int ->Int -> Board
emptyBoard x y = Board PH x y [] 0

impossibleMoves :: [(Int, Int)] -> [Move]
impossibleMoves []            = []
impossibleMoves ((x, y) : ys) = [H x y] ++ [H (x-1) y] ++ [V x (y-1)] ++ [V x y] ++ impossibleMoves ys

validMoves :: Int -> Int -> [(Int, Int)] -> [Move]
validMoves x y zs = (allMoves x y) \\ (impossibleMoves zs)

toBoard :: (Int, Int, [(Int,Int)], Player) -> Board
toBoard (xBound, yBound, xs, p) = Board p xBound yBound xs 0

-- We also need to perform the opposite conversion, from your
-- representation of boards, to our naive description of boards, so
-- that we can "read" your boards whenever we need to, for testing
-- purposes:

fromBoard :: Board -> (Int, Int, [(Int, Int)], Player)
fromBoard (Board p x y xs _) = (x, y, xs, p)

-- Similarly, given a Board, we want to create a Move given
-- (x,y,player) where (x,y) is a position in the Board:

toMove :: Board -> (Int, Int, Player) -> Move
toMove (Board p _ _ _ _) (x, y, PV) = (V x y)
toMove (Board p _ _ _ _) (x, y, PH) = (H x y)

-- And we want to do the opposite too:

fromMove :: Move -> (Int, Int, Player)
fromMove (V x y) = (x, y, PV)
fromMove (H x y) = (x, y, PH)

fromMove' :: Board -> Move -> (Int, Int, Player)
fromMove' b = fromMove

-- The first exercise is to play an allowed move in a board, and get
-- the resulting board. Simply throw an error if the move is not
-- allowed in that board. We will only test your function with allowed
-- moves:
insert' :: Move -> Board -> Board
insert' (V x y) (Board PV xBound yBound ops o) = Board PH xBound yBound ([(x, y)] ++ [(x, (y+1))] ++ ops) o
insert' (H x y) (Board PH xBound yBound ops o) = Board PV xBound yBound ([(x, y)] ++ [((x+1), y)] ++ ops) o

wins :: Board -> Bool
wins board = if allowedMoves board == [] then True else False

play :: Move -> Board -> Board
play m (Board p x y ops o) = insert' m (Board p x y ops o) 
--in
--    if wins phs
--        then phs
--        else Board PV x y ops (-1)
--play m (Board PV x y ops o) = let pvs = insert' m (Board PV x y ops o) in
--    if wins pvs
--        then pvs
--        else pvs
--play m board = error "Bug when using function play."

-- Ah. But what are the allowed moves in a give board? You tell me:

allowedMoves :: Board -> [Move]
allowedMoves (Board PH x y ops _) = [(H x' y') | (H x' y') <- (validMoves x y ops)]             
allowedMoves (Board PV x y ops _) = [(V x' y') | (V x' y') <- (validMoves x y ops)]

-- Now build the tree of a game. You are allowed to base your
-- implementation on any of the given treeOf implementations for the
-- several tic-tac-toe programs in Canvas (discussed in the lectures):

treeOf :: Board -> Tree
treeOf board = Fork board forest
  where
    forest :: [(Move,Tree)]
    forest 
      | wins board = [] 
      | otherwise     = [(m, treeOf(play m board)) | m <- allowedMoves board]

-- Now we want to have the computer playing first, lazily against an
-- opponent. The opponent supplies the list of moves. But the computer
-- is not allowed to cheat. It has to play its first move without
-- looking at any of the moves of the opponent:


optimalOutcome :: Tree -> Outcome
optimalOutcome (Fork board []) = outcome board 
optimalOutcome (Fork board forest) 
   | player board == PH = maximum optimalOutcomes
   | otherwise          = minimum optimalOutcomes
 where 
   optimalOutcomes = [optimalOutcome tree | (_,tree) <- forest]


computerFirst :: Tree -> [Move] -> [Move]
computerFirst  = undefined

-- And now you want the computer to play second. It will have to first
-- check the head move of the opponent, provided the list of moves is
-- non-empty, and base its first move (the head of the output list) on
-- that:

computerSecond :: Tree -> [Move] -> [Move]
computerSecond = undefined

-- This should be done so that the following example works:

iplay :: ([Move]->[Move]) -> ([Move]->[Move]) -> [Move]
iplay f g = intercalate' ys xs
  where
    ys = f xs
    xs = g ys

intercalate' :: [a] -> [a] -> [a]
intercalate' []     ys = ys 
intercalate' (x:xs) ys = x : intercalate' ys xs

-- What the following example should do is produce the list of moves
-- that results from having the computer playing against itself:

example :: Tree -> [Move]
example tree = iplay (computerFirst tree) (computerSecond tree)

-- We now move to random playing. The randomness monad we used for
-- quick sort in the lecture is not sufficiently lazy for our
-- purposes. We work with a lazy Random monad based on
--
--   https://hackage.haskell.org/package/MonadRandomLazy-0.1/docs/Control-Monad-LazyRandom.html
--
-- instead, define below.  


-- We use the standard random generator as our type of seeds for
-- random things:

type Seed = StdGen

-- We get seeds for random-thing generation from Int's:

mkSeed :: Int -> Seed
mkSeed = mkStdGen

-- See https://en.wikipedia.org/wiki/Random_seed
-- We define the monad as follows:

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )

-- The following are to "get out" this monad:

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

-- What this says is that if you have a random element of type a (that
-- is, something of type LRand a), one way to get something of type a
-- is to provide a seed.

-- This is like the above, but also produces a new seed, if we need it:

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- And finally we need to be able to generate random elements:

getRandom :: Random a => LRand a
getRandom = LRand $ fst . random

-- But this needs a to be in the Random type class. Most types are
-- automatically there, and it is unlikely you will need to worry
-- about this in this exercise, unless you do very sophisticated
-- things.

-- We also may need to get random elements within a range:

getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range

-- This is the end of our definition of our lazy randomness monad.

randomFirst :: Tree -> [Move] -> LRand [Move]
randomFirst = undefined

randomSecond :: Tree -> [Move] -> LRand [Move]
randomSecond = undefined

computerFirstHeuristic :: Board -> [Move] -> [Move]
computerFirstHeuristic  = undefined

computerSecondHeuristic :: Board -> [Move] -> [Move]
computerSecondHeuristic = undefined

--Appendix
outcomes :: Tree -> [Outcome]
outcomes (Fork board []) = [outcome board]
outcomes (Fork _ trees) = concat [outcomes tree | (_,tree) <- trees]