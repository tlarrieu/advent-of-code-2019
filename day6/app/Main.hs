module Main where

import Common
import Control.Monad (liftM2)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import Data.Tree (Forest, Tree(Node), Tree, levels, unfoldTree)

type Object = String

main :: IO ()
main = do
  solve (6, 1) `with` countOrbits
  solve (6, 2) `with` distance "YOU" "SAN"

with :: Action Int -> (Tree Object -> Int) -> IO ()
with = (. (. (fromRoot "COM" . parse)))
  where
    fromRoot label xs = unfoldTree (\x -> (x, fromMaybe [] (xs !? x))) label
    parse = foldr (append . orbit) Map.empty . lines
    append (k, v) = Map.insertWith (++) k [v]
    orbit = liftM2 (,) head (!! 1) . splitOn ")"

countOrbits :: Tree Object -> Int
countOrbits = sum . zipWith (\i xs -> i * length xs) [0 ..] . levels

distance :: Object -> Object -> Tree Object -> Int
distance a b node = length $ u \\ i
  where
    (xs, ys) = (pathTo a node, pathTo b node)
    (u, i) = (xs `union` ys, xs `intersect` ys)

pathTo :: Object -> Tree Object -> [Object]
pathTo = pathTo' []
  where
    pathTo' xs needle node
      | null xs = pathTo' [object node] needle node
      | needle == object node = reverse . tail $ xs
      | isLeaf node = []
      | otherwise =
        concatMap
          (\node' -> pathTo' (object node' : xs) needle node')
          (children node)

isLeaf :: Tree Object -> Bool
isLeaf = null . children

object :: Tree Object -> Object
object (Node r _) = r

children :: Tree Object -> Forest Object
children (Node _ c) = c
