{-# LANGUAGE NamedFieldPuns #-}

module Computer
  ( Computer(Computer)
  , build
  , isHalt
  , isPrint
  , isSave
  , output
  , run
  , setInput
  ) where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!), (//), fromList)
import qualified Data.Vector as V

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

data Mode
  = Ref
  | Val

type State = Vector Int

type Pointer = Int

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

type Predicate = Computer -> Bool

build :: [Int] -> Computer
build xs = Computer {input = [], output = [], pointer = 0, state = fromList xs}

run :: Predicate -> Computer -> Computer
run predicate = (!! 1) . dropWhile predicate . iterate next

isPrint :: Computer -> Bool
isPrint = isPrint' . instruction
  where
    isPrint' (Print _) = True
    isPrint' _ = False

isHalt :: Computer -> Bool
isHalt = isHalt' . instruction
  where
    isHalt' Halt = True
    isHalt' _ = False

isSave :: Computer -> Bool
isSave = isSave' . instruction
  where
    isSave' (Save _) = True
    isSave' _ = False

setInput :: [Input] -> Computer -> Computer
setInput xs comp = comp {input = xs}

next :: Computer -> Computer
next = instruction >>= next'
  where
    next' Halt = id
    next' (Add (a, b, i)) = move 4 . update (i, a + b)
    next' (Mult (a, b, i)) = move 4 . update (i, a * b)
    next' (Save i) = move 2 . save i
    next' (Print i) = move 2 . write i
    next' (JumpTrue (a, i)) =
      if a /= 0
        then jump i
        else move 3
    next' (JumpFalse (a, i)) =
      if a == 0
        then jump i
        else move 3
    next' (LessThan (a, b, i)) =
      move 4 .
      if a < b
        then update (i, 1)
        else update (i, 0)
    next' (Equals (a, b, i)) =
      move 4 .
      if a == b
        then update (i, 1)
        else update (i, 0)
    move i comp = jump (pointer comp + i) comp
    jump i comp = comp {pointer = i}
    save _ Computer {input = []} = error "No more input available"
    save i comp@Computer {input = (x:xs)} = update (i, x) (comp {input = xs})
    write i comp = comp {output = state comp ! i : output comp}
    update xs comp = comp {state = state comp // [xs]}

instruction :: Computer -> Instruction
instruction Computer {state, pointer} =
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
    expand _ Val = id
    expand s Ref = (s !)
    mode 0 = Just Ref
    mode 1 = Just Val
    mode _ = Nothing
