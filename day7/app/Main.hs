{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Common
import Data.Char (digitToInt)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

type Pointer = Int

data Mode
  = Ref
  | Val

data Instruction
  = Mult (Int, Int, Int)
  | Add (Int, Int, Int)
  | Save Int
  | Print Int
  | JumpTrue (Int, Int)
  | JumpFalse (Int, Int)
  | LessThan (Int, Int, Int)
  | Equals (Int, Int, Int)
  | Halt
  deriving (Eq)

type State = Vector Int

type Input = Int

type Output = Int

data Computer =
  Computer
    { input :: [Input]
    , output :: [Output]
    , pointer :: Pointer
    , state :: State
    }
  deriving (Show)

data Amplifiers =
  Amplifiers
    { active :: Int
    , computers :: Vector Computer
    }

type Predicate = Computer -> Bool

main :: IO ()
main = do
  solve (7, 1) (bestThrust . initialize)
  solve (7, 2) (bestThrust . initialize)

bestThrust :: Computer -> Int
bestThrust = maximum . flip map (permutations [0 .. 4]) . amps

amps :: Computer -> [Input] -> Int
amps comp [i, j, k, l, m] = signal e
  where
    a = amp (i, 0) comp
    b = amp (j, signal a) comp
    c = amp (k, signal b) comp
    d = amp (l, signal c) comp
    e = amp (m, signal d) comp
    amp (x, y) = run (not . isHalt) . setInput [x, y]
amps _ _ = error "Bad phases input, it should contain exactly 5 elements"

signal :: Computer -> Int
signal = head . output

run :: Predicate -> Computer -> Computer
run predicate = (!! 1) . dropWhile predicate . iterate next

next :: Computer -> Computer
next = nextInstruction >>= nextState

nextState :: Instruction -> Computer -> Computer
nextState Halt = id
nextState (Add (a, b, i)) = move 4 . update (i, a + b)
nextState (Mult (a, b, i)) = move 4 . update (i, a * b)
nextState (Save i) = move 2 . save i
nextState (Print i) = move 2 . write i
nextState (JumpTrue (a, i)) =
  if a /= 0
    then jump i
    else move 3
nextState (JumpFalse (a, i)) =
  if a == 0
    then jump i
    else move 3
nextState (LessThan (a, b, i)) =
  move 4 .
  if a < b
    then update (i, 1)
    else update (i, 0)
nextState (Equals (a, b, i)) =
  move 4 .
  if a == b
    then update (i, 1)
    else update (i, 0)

isPrint :: Computer -> Bool
isPrint = isPrint' . nextInstruction
  where
    isPrint' (Print _) = True
    isPrint' _ = False

isHalt :: Computer -> Bool
isHalt = isHalt' . nextInstruction
  where
    isHalt' Halt = True
    isHalt' _ = False

move :: Pointer -> Computer -> Computer
move i comp = jump (pointer comp + i) comp

jump :: Pointer -> Computer -> Computer
jump i comp = comp {pointer = i}

save :: Pointer -> Computer -> Computer
save _ Computer {input = []} = error "No more input available"
save i comp@Computer {input = (x:xs)} = update (i, x) (comp {input = xs})

write :: Pointer -> Computer -> Computer
write i comp = comp {output = state comp ! i : output comp}

update :: (Int, Int) -> Computer -> Computer
update xs comp = comp {state = state comp // [xs]}

nextInstruction :: Computer -> Instruction
nextInstruction Computer {state, pointer} =
  case mkOp op :: Int of
    1 -> Add (vals ! 0, vals ! 1, vars ! 2)
    2 -> Mult (vals ! 0, vals ! 1, vars ! 2)
    3 -> Save (vars ! 0)
    4 -> Print (vars ! 0)
    5 -> JumpTrue (vals ! 0, vals ! 1)
    6 -> JumpFalse (vals ! 0, vals ! 1)
    7 -> LessThan (vals ! 0, vals ! 1, vars ! 2)
    8 -> Equals (vals ! 0, vals ! 1, vars ! 2)
    99 -> Halt
    _ -> error "Unknown operation"
  where
    vals = V.zipWith (expand state) (V.fromList $ mkModes modes) vars
    vars = V.drop (pointer + 1) state
    mkOp = read . reverse
    mkModes = (++ repeat Ref) . mapMaybe (mode . digitToInt)
    (op, modes) = splitAt 2 . reverse . show . (! pointer) $ state

expand :: State -> Mode -> Int -> Int
expand _ Val = id
expand state Ref = (state !)

mode :: Int -> Maybe Mode
mode 0 = Just Ref
mode 1 = Just Val
mode _ = Nothing

setInput :: [Input] -> Computer -> Computer
setInput xs comp = comp {input = xs}

initialize :: String -> Computer
initialize xs =
  Computer
    { input = []
    , output = []
    , pointer = 0
    , state = V.fromList . map (\x -> read x :: Int) . splitOn "," $ xs
    }
