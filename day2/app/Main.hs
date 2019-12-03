module Main where

import Common
import Data.Bool
import Data.List.Split
import Data.Maybe
import Data.Vector (Vector, (!), (//), fromList)

type Address = Int

type Noun = Int

type Verb = Int

type Input = (Noun, Verb)

type Output = Int

data Instruction
  = Mult (Address, Address, Address)
  | Add (Address, Address, Address)
  | Id
  | Unknown

type State = Vector Int

main :: IO ()
main =
  let fromString = fromList . map (\x -> read x :: Int) . splitOn ","
      checksum (a, b) = 100 * a + b
   in do solve (2, 1) (output (12, 2) . fromString)
         solve (2, 2) (checksum . input 19690720 . fromString)

input :: Output -> State -> Input
input result state =
  head $ mapMaybe try [(i, j) | i <- [0 .. 99], j <- [0 .. 99]]
  where
    try x = bool Nothing (Just x) (output x state == result)

output :: Input -> State -> Output
output = (((! 0) . run) .) . initialize
  where
    initialize (a, b) = (// [(1, a), (2, b)])
    run = run' 0
    run' addr state =
      case instruction addr state of
        Id -> state
        instr -> run' (addr + 4) (compute instr state)

instruction :: Address -> State -> Instruction
instruction addr state = res
  where
    opcode = state ! addr
    (x, y, i) = (state ! (addr + 1), state ! (addr + 2), state ! (addr + 3))
    res
      | opcode == 1 = Add (x, y, i)
      | opcode == 2 = Mult (x, y, i)
      | opcode == 99 = Id
      | otherwise = Unknown

compute :: Instruction -> State -> State
compute (Add (a, b, i)) state = state // [(i, state ! a + state ! b)]
compute (Mult (a, b, i)) state = state // [(i, state ! a * state ! b)]
compute Id state = state
compute Unknown _ = undefined -- We should handle this better
