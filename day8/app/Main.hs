module Main where

import Common
import Control.Monad
import Data.Char (digitToInt, intToDigit)
import Data.List (minimumBy, transpose)
import Data.List.Split (chunksOf)

type Layer = [Int]

type SIF = [Layer]

main :: IO ()
main = do
  solve
    (8, 1)
    (show .
     checksum .
     snd .
     minimumBy (\(a, _) (b, _) -> compare a b) .
     map (length . filter (== 0) >>= (,)) . parse)
  solve
    (8, 2)
    (("\n" ++) .
     unlines .
     chunksOf 25 . map (colorize . intToDigit . pixel) . transpose . parse)

checksum :: Layer -> Int
checksum = ap ((*) . length . filter (1 ==)) (length . filter (2 ==))

colorize :: Char -> Char
colorize '0' = ' '
colorize '1' = '#'
colorize _ = error "Bad pixel"

pixel :: [Int] -> Int
pixel [] = 2
pixel (0:_) = 0
pixel (1:_) = 1
pixel (2:xs) = pixel xs
pixel (x:_) = error $ "Unknow color " ++ show x

parse :: String -> SIF
parse = chunksOf (25 * 6) . map digitToInt . head . lines
