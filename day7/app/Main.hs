{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List (permutations)
import Data.List.Split (splitOn)

import Amplifiers (AmpType(..), process)
import qualified Amplifiers as A
import Common
import Computer (Computer)
import qualified Computer as C

main :: IO ()
main = do
  solve (7, 1) (maximum . thrusts SinglePass [0 .. 4] . initialize)
  solve (7, 2) (maximum . thrusts FeedbackLoop [5 .. 9] . initialize)

thrusts :: AmpType -> [Int] -> Computer -> [Int]
thrusts atype settings comp =
  map (\x -> process (A.build atype x comp) 0) (permutations settings)

initialize :: String -> Computer
initialize = C.build . map (\x -> read x :: Int) . splitOn ","
