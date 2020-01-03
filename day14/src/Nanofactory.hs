{-# LANGUAGE NamedFieldPuns #-}

module Nanofactory
  ( Factory(Factory)
  , Product
  , Leftovers
  , initialize
  , leftovers
  , oresPerFuel
  , state
  , process
  , reactions
  , reduce
  , totalFuel
  ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map (Map, (!?))
import Data.Maybe

type Chemical = String

type Product = (Chemical, Int)

type Formula = (Product, [Product])

type Formulas = Map Chemical Formula

type Leftovers = Map Chemical Product

data Factory =
  Factory
    { state :: [Product]
    , reactions :: Formulas
    , leftovers :: Leftovers
    }
  deriving (Show, Eq)

initialize :: String -> Factory
initialize xs =
  Factory
    { state = [("FUEL", 1)]
    , reactions = reactions xs
    , leftovers = M.empty
    }
  where
    reactions = M.fromList . map (reaction . products) . lines
    reaction (y@(chem, _):ys) = (chem, (y, (shrink . reverse) ys))
    reaction _ = error "can't build from empty product list"
    products = reverse . map new . tokenize
    new = (\[i, chem] -> (chem, read i)) . splitOn " "
    tokenize = (>>= splitOn " => ") . splitOn ", "

oresPerFuel :: Factory -> Int
oresPerFuel = fromJust . oreCount . run 1

totalFuel :: Factory -> Int
totalFuel = liftM3 totalFuel' id guess oresPerFuel
  where
    guess = (trillion `div`) . oresPerFuel

totalFuel' :: Factory -> Int -> Int -> Int
totalFuel' f fuel opf
  | 0 <= stock && stock < opf = fuel
  | stock > 0 = totalFuel' f (fuel + increment) opf
  | otherwise = totalFuel' f (fuel - increment) opf
  where
    stock = trillion - (fromJust .oreCount . run fuel) f
    increment = stock `div` opf

run :: Int -> Factory -> Factory
run fuel = head . dropWhile (isNothing . oreCount) . iterate reduce . reset fuel
  where
    reset i f = f {state = [("FUEL", i)]}

oreCount :: Factory -> Maybe Int
oreCount Factory {state = [("ORE", i)]} = Just i
oreCount _ = Nothing

reduce :: Factory -> Factory
reduce f@Factory {state, reactions, leftovers} =
  f {state = state', leftovers = leftovers'}
  where
    (state', leftovers') = reduce' reactions [] state leftovers

reduce' ::
     Formulas -> [Product] -> [Product] -> Leftovers -> ([Product], Leftovers)
reduce' _ acc [] l = (shrink acc, l)
reduce' reactions acc (x@(chem, _):xs) l = reduce' reactions acc' xs l''
  where
    (x', l') = applyLeftovers x l
    (acc', l'') =
      case process x' (reactions !? chem) of
        Nothing -> (acc, l')
        Just (products, Nothing) -> (acc ++ products, l')
        Just (products, Just p@(chem', _)) ->
          (acc ++ products, M.insert chem' p l')

applyLeftovers :: Product -> Leftovers -> (Maybe Product, Leftovers)
applyLeftovers p@(chem, _) leftovers = (p', leftovers')
  where
    l = leftovers !? chem
    (p', l') = applyLeftovers' p l
    leftovers' = M.update (const l') chem leftovers

applyLeftovers' :: Product -> Maybe Product -> (Maybe Product, Maybe Product)
applyLeftovers' p Nothing = (Just p, Nothing)
applyLeftovers' (a, i) (Just (b, j))
  | a /= b = error "Can't compute remainders for different products"
  | i < j = (Nothing, Just (a, j - i))
  | i > j = (Just (a, i - j), Nothing)
  | otherwise = (Nothing, Nothing)

shrink :: [Product] -> [Product]
shrink = map compress . rearrange

compress :: [Product] -> Product
compress = foldr (\(chem, i) (_, j) -> (chem, i + j)) ("", 0)

rearrange :: [Product] -> [[Product]]
rearrange = groupBy (\(chem, _) (chem', _) -> chem == chem') . sort

process :: Maybe Product -> Maybe Formula -> Maybe ([Product], Maybe Product)
process Nothing _ = Nothing
process (Just p) Nothing = Just ([p], Nothing)
process (Just (c, i)) (Just ((c', i'), xs))
  | c /= c' = Nothing
  | otherwise =
    Just
      ( map (second (i /// i' *)) xs
      , case (i /// i') * i' - i of
          0 -> Nothing
          k -> Just (c, k))

trillion :: Int
trillion = 10 ^ (12 :: Int)

(///) :: Int -> Int -> Int
(///) a b =
  a `div` b +
  (if a `mod` b == 0
     then 0
     else 1)
