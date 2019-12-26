module Main where

import Data.List.Split

import Common
import Moon

type System = [Moon]

main :: IO ()
main = solve (12, 1) (checksum . (!! 1000) . iterate step . parse)

checksum :: System -> Int
checksum = sum . map energy

step :: System -> System
step = map move . (flip fall >>= map)

parse :: String -> System
parse = map (build . map read . splitOn ",") . lines
