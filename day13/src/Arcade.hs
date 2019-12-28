{-# LANGUAGE NamedFieldPuns #-}

module Arcade
  ( Arcade
  , Tile(..)
  , ball
  , blocks
  , new
  , paddle
  , play
  , score
  , screen
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Computer (Computer, State(..))
import qualified Computer as C

type Coords = (Int, Int)

data Arcade =
  Arcade
    { program :: Computer
    , screen :: Map Coords Tile
    , ball :: Maybe Coords
    , paddle :: Maybe Coords
    , score :: Int
    }
  deriving (Show)

data Tile
  = Empty
  | Wall
  | Block
  deriving (Eq, Enum, Show)

new :: [Int] -> Arcade
new xs =
  Arcade
    { program = C.new xs
    , screen = M.empty
    , ball = Nothing
    , paddle = Nothing
    , score = 0
    }

play :: Arcade -> Arcade
play = C.state . program >>= play'
  where
    play' Done = id
    play' Input = play . next . tilt
    play' Output = play . parse . next
    play' Working = play . next
    next a = a {program = (C.next . program) a}

tilt :: Arcade -> Arcade
tilt a@Arcade {ball, paddle} = a {program = program' a}
  where
    program' = C.setInput [fromMaybe 0 (joystick ball paddle)] . program
    joystick b p = do
      (x, _) <- b
      (x', _) <- p
      pure (joystick' x x')
    joystick' x x'
      | x > x' = -1
      | x < x' = 1
      | otherwise = 0

parse :: Arcade -> Arcade
parse a =
  case (C.output . program) a of
    [s, 0, -1] -> clear a {score = s}
    [3, y, x] -> clear a {ball = Just (x, y)}
    [4, y, x] -> clear a {paddle = Just (x, y)}
    [t, y, x] -> clear a {screen = M.insert (x, y) (toEnum t) (screen a)}
    _ -> a
  where
    clear b = b {program = (program b) {C.output = []}}

blocks :: Arcade -> [Coords]
blocks = M.keys . M.filter (== Block) . screen
