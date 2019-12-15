{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List.Split (splitOn)

import Common
import Computer (Computer)
import qualified Computer as C

main :: IO ()
main = do
  solve
    (9, 1)
    (head . C.output . C.run (not . C.isHalt) . C.setInput [1] . initialize)
  solve
    (9, 2)
    (head . C.output . C.run (not . C.isHalt) . C.setInput [2] . initialize)

initialize :: String -> Computer
initialize = C.build . map (\x -> read x :: Int) . splitOn ","
