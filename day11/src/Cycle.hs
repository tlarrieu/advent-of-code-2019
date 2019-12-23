module Cycle
  ( pred
  , succ
  ) where

import Prelude hiding (pred, succ)

succ :: (Enum a, Bounded a) => a -> a
succ = rot 1

pred :: (Enum a, Bounded a) => a -> a
pred = rot (-1)

rot :: (Enum a, Bounded a) => Int -> a -> a
rot n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where
    add m x y = (x + y + m) `rem` m
