module Main where

import Common
import Data.List (group)
import Data.List.Split (splitOn)

type Range = (Int, Int)

type Predicate = String -> Bool

main :: IO ()
main = do
  solve (4, 1) `with` [sameDigits (>= 2), increasing]
  solve (4, 2) `with` [sameDigits (== 2), increasing]

with :: Action Int -> [Predicate] -> IO ()
with action predicates = action $ length . select predicates . build . parse
  where
    select p = filter (and . sequenceA p . show)

sameDigits :: (Int -> Bool) -> String -> Bool
sameDigits f = any f . map length . group

increasing :: String -> Bool
increasing [] = True
increasing [_] = True
increasing (x:y:xs) = y >= x && increasing (y : xs)

build :: (Int, Int) -> [Int]
build (a, b) = [a .. b]

parse :: String -> (Int, Int)
parse = (\[a, b] -> (read a, read b)) . splitOn "-"
