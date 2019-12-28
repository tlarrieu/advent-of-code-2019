module Main where

import Data.List.Split (splitOn)

import Arcade
import Common

main :: IO ()
main = do
  solve (13, 1) (length . blocks . play . new . initialize)
  solve (13, 2) (score . play . new . (\(_:xs) -> 2 : xs) . initialize)

initialize :: String -> [Int]
initialize = map (\x -> read x :: Int) . splitOn ","
