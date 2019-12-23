module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as M

import Common
import Robot

main :: IO ()
main = do
  solve (11, 1) (show . checksum . run . initialize Black)
  solve (11, 2) (("\n" ++) . message . run . initialize White)

checksum :: Robot -> Int
checksum = length . M.keys . panels

initialize :: Color -> String -> Robot
initialize color = build color . map (\x -> read x :: Int) . splitOn ","
