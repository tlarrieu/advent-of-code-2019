{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List.Split (splitOn)

import Common
import Computer (Computer)
import qualified Computer as C

main :: IO ()
main = do
  solve (9, 1) initialize

initialize :: String -> Computer
initialize = C.build . map (\x -> read x :: Int) . splitOn ","
