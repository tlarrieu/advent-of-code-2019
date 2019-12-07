module Main where

import Common
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)

data Direction
  = U
  | L
  | R
  | D
  deriving (Show)

type Move = (Direction, Int)

type Point = (Int, Int)

type Wire = [Point]

main :: IO ()
main = do
  solve (3, 1) `with` d1
  solve (3, 2) `with` d2

with :: Action Int -> (Map Point Int -> [Int]) -> IO ()
with action f = action $ minimum . f . (\[a, b] -> a &&& b) . build . parse

d1 :: Map Point Int -> [Int]
d1 = filter (> 0) . map (manhattan (0, 0)) . Map.keys

d2 :: Map Point Int -> [Int]
d2 = filter (> 0) . Map.elems

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x', y') = abs (x' - x) + abs (y' - y)

(&&&) :: Wire -> Wire -> Map Point Int
(&&&) a b = Map.intersectionWith (+) (toMap a) (toMap b)
  where
    toMap = Map.fromList . (`zip` [(0 :: Int) ..])

build :: [[Maybe Move]] -> [Wire]
build = map (concat . reverse . foldl' folder [[origin]])
  where
    origin = (0, 0)
    folder [] _ = []
    folder xs Nothing = xs
    folder (x:xs) (Just move) = translate (last x) move : x : xs

translate :: Point -> Move -> [Point]
translate (x, y) (L, i) = [(x - j, y) | j <- [1 .. i]]
translate (x, y) (R, i) = [(x + j, y) | j <- [1 .. i]]
translate (x, y) (U, i) = [(x, y + j) | j <- [1 .. i]]
translate (x, y) (D, i) = [(x, y - j) | j <- [1 .. i]]

parse :: String -> [[Maybe Move]]
parse = map (map parse' . splitOn ",") . lines
  where
    parse' ('L':xs) = Just (L, read xs)
    parse' ('R':xs) = Just (R, read xs)
    parse' ('U':xs) = Just (U, read xs)
    parse' ('D':xs) = Just (D, read xs)
    parse' _ = Nothing
