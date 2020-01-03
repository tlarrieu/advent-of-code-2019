{-# LANGUAGE FlexibleContexts #-}

module Main where

import Common
import Nanofactory

main :: IO ()
main = do
  solve (14, 1) (oresPerFuel . initialize)
  solve (14, 2) (totalFuel . initialize)
