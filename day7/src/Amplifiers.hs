{-# LANGUAGE NamedFieldPuns #-}

module Amplifiers
  ( Amplifiers(Amplifiers)
  , AmpType(SinglePass, FeedbackLoop)
  , active
  , build
  , amptype
  , computers
  , process
  ) where

import Computer hiding (build)
import Control.Applicative (liftA2)
import Data.Vector (Vector, (!), (//), fromList)

data Amplifiers =
  Amplifiers
    { amptype :: AmpType
    , active :: Int
    , computers :: Vector Computer
    }

data AmpType
  = SinglePass
  | FeedbackLoop

build :: AmpType -> [Int] -> Computer -> Amplifiers
build atype inputs = Amplifiers atype 0 . fromList . list
  where
    list =
      zipWith (\i -> run (not . isSave) . setInput [i]) inputs . replicate 5

process :: Amplifiers -> Int -> Int
process Amplifiers {amptype, active, computers} sig
  | isHalt $ computers ! 4 = signal $ computers ! 4
  | otherwise =
    process
      (Amplifiers amptype ((active + 1) `mod` 5) (computers // [(active, comp)]))
      (signal comp)
  where
    comp = run' amptype . setInput [sig] $ computers ! active
    run' SinglePass = run (not . isHalt)
    run' FeedbackLoop = run (not . liftA2 (||) isPrint isHalt)

signal :: Computer -> Int
signal = head . output
