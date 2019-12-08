{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Common
import Data.Char
import Data.List.Split
import Data.Maybe
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

type State = Vector Int

type Input = Int

type Output = Int

data Computer =
  Computer
    { input :: Maybe Input
    , output :: [Output]
    , pointer :: Pointer
    , state :: State
    }
  deriving (Show)

main :: IO ()
main = do
  solve (5, 1) (head . output . run . initialize 1)
  solve (5, 2) (head . output . run . initialize 5)

run :: Computer -> Computer
run = nextInstruction >>= next'
  where
    next' Halt = id
    next' instr = run . next instr

next :: Instruction -> Computer -> Computer
next Halt = id
next (Add (a, b, i)) = move 4 . update (i, a + b)
next (Mult (a, b, i)) = move 4 . update (i, a * b)
next (Save i) = move 2 . save i
next (Print i) = move 2 . write i
next (JumpTrue (a, i)) =
  if a /= 0
    then jump i
    else move 3
next (JumpFalse (a, i)) =
  if a == 0
    then jump i
    else move 3
next (LessThan (a, b, i)) =
  move 4 .
  if a < b
    then update (i, 1)
    else update (i, 0)
next (Equals (a, b, i)) =
  move 4 .
  if a == b
    then update (i, 1)
    else update (i, 0)

move :: Pointer -> Computer -> Computer
move i comp = jump (pointer comp + i) comp

jump :: Pointer -> Computer -> Computer
jump i comp = comp {pointer = i}

save :: Pointer -> Computer -> Computer
save i comp@Computer {input = Just input} = update (i, input) comp
save _ _ = error "No input provided"

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
expand _ Val i = i
expand state Ref i = state ! i

mode :: Int -> Maybe Mode
mode 0 = Just Ref
mode 1 = Just Val
mode _ = Nothing

initialize :: Input -> String -> Computer
initialize i xs =
  Computer
    { input = Just i
    , output = []
    , pointer = 0
    , state = V.fromList . map (\x -> read x :: Int) . splitOn "," $ xs
    }
