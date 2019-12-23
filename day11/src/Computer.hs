{-# LANGUAGE NamedFieldPuns #-}

module Computer
  ( Computer(Computer)
  , build
  , isHalt
  , isPrint
  , isSave
  , output
  , run
  , next
  , setInput
  , clearOutput
  , state
  ) where

import Data.Char (digitToInt)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.Maybe (mapMaybe)

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
  | Base Int
  deriving (Eq, Show)

data Mode
  = Ref
  | Val
  | Rel
  deriving (Show)

type State = IntMap Int

type Pointer = Int

type Input = Int

type Output = Int

data Computer =
  Computer
    { base :: Int
    , input :: [Input]
    , output :: [Output]
    , pointer :: Pointer
    , state :: State
    }
  deriving (Show)

type Predicate = Computer -> Bool

build :: [Int] -> Computer
build xs =
  Computer
    { base = 0
    , input = []
    , output = []
    , pointer = 0
    , state = M.fromList $ zip [0 ..] xs
    }

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

clearOutput :: Computer -> Computer
clearOutput comp = comp {output = []}

next :: Computer -> Computer
next = instruction >>= next'
  where
    next' Halt = id
    next' (Base i) = move 2 . incBase i
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
    write i comp = comp {output = i : output comp}
    update (i, x) comp = comp {state = M.insert i x (state comp)}
    incBase i comp@Computer {base} = comp {base = base + i}

instruction :: Computer -> Instruction
instruction Computer {state, pointer, base} =
  case op of
    1 -> Add (head vals, vals !! 1, addr !! 2)
    2 -> Mult (head vals, vals !! 1, addr !! 2)
    3 -> Save (head addr)
    4 -> Print (head vals)
    5 -> JumpTrue (head vals, vals !! 1)
    6 -> JumpFalse (head vals, vals !! 1)
    7 -> LessThan (head vals, vals !! 1, addr !! 2)
    8 -> Equals (head vals, vals !! 1, addr !! 2)
    9 -> Base (head vals)
    99 -> Halt
    _ -> error "Unknown operation"
  where
    (op, modes) = split $ state ! pointer
    vals = zipWith interpret modes raw
    addr = zipWith literal modes raw
    raw = drop (pointer + 1) (M.elems state)
    interpret Ref = (state !!!)
    interpret Val = id
    interpret Rel = (state !!!) . (+ base)
    literal Ref = id
    literal Val = id
    literal Rel = (+ base)
    (!!!) = flip (M.findWithDefault 0)

split :: Int -> (Int, [Mode])
split i =
  (read . reverse $ op, mapMaybe (mode . digitToInt) modes ++ repeat Ref)
  where
    (op, modes) = splitAt 2 . reverse . show $ i
    mode 0 = Just Ref
    mode 1 = Just Val
    mode 2 = Just Rel
    mode _ = Nothing
