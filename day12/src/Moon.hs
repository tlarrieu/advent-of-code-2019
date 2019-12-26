{-# LANGUAGE NamedFieldPuns #-}

module Moon
  ( Moon(Moon)
  , build
  , energy
  , fall
  , move
  , pos
  , vel
  ) where

import Control.Monad (liftM2)

import Math

data Moon =
  Moon
    { pos :: Vect3D
    , vel :: Vect3D
    }
  deriving (Show, Eq, Ord)

build :: [Int] -> Moon
build [x, y, z] = Moon {pos = Vect3D (x, y, z), vel = Vect3D (0, 0, 0)}
build _ = error "Something went wrong with the input file"

energy :: Moon -> Int
energy = liftM2 (*) kinetic potential
  where
    kinetic = manhattan . vel
    potential = manhattan . pos

fall :: Moon -> [Moon] -> Moon
fall m@(Moon p v) = (\x -> Moon p (v <> x)) . foldr1 (<>) . map (diff m)
  where
    diff (Moon (Vect3D (x, y, z)) _) (Moon (Vect3D (x', y', z')) _) =
      Vect3D (delta x x', delta y y', delta z z')
    delta a b
      | a == b = 0
      | a < b = 1
      | otherwise = -1

move :: Moon -> Moon
move Moon {pos, vel} = Moon (pos <> vel) vel
