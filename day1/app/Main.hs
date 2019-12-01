module Main where

import Common
import Data.List (unfoldr)

main :: IO ()
main = do
  solve (1, 1) `with` fuel
  solve (1, 2) `with` fuel'

with ::
     Read a
  => Num a =>
       Action a -> (a -> a) -> IO ()
action `with` func = action $ sum . map (func . read)

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

fuel' :: Int -> Int
fuel' = sum . tail . unfoldr maybeFuel
  where
    maybeFuel b
      | b <= 0 = Nothing
      | otherwise = Just (b, fuel b)
