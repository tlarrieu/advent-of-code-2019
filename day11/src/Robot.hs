{-# LANGUAGE NamedFieldPuns #-}

module Robot
  ( Robot(Robot)
  , Color(Black, White)
  , build
  , message
  , panels
  , run
  ) where

import qualified Data.Map as M
import Data.Map (Map)

import Computer (Computer)
import qualified Computer as C
import qualified Cycle

data Orientation
  = North
  | East
  | South
  | West
  deriving (Show, Enum, Bounded)

data Turn
  = L
  | R
  deriving (Show, Enum)

data Color
  = Black
  | White
  deriving (Show, Enum)

type Position = (Int, Int)

type Instruction = (Int, Int)

data Robot =
  Robot
    { brain :: Computer
    , orientation :: Orientation
    , position :: Position
    , panels :: Map Position Color
    }
  deriving (Show)

build :: Color -> [Int] -> Robot
build initialColor xs =
  setInput .
  paint initialColor $
  Robot
    { brain = C.build xs
    , orientation = minBound
    , position = (0, 0)
    , panels = M.empty
    }

run :: Robot -> Robot
run robot
  | isDone robot = robot
  | isPaint robot = run' . apply (color, direction) $ robot
  | otherwise = run' robot
  where
    run' r = run $ r {brain = C.next . brain $ r}
    [direction, color] = C.output . brain $ robot
    isPaint = (== 2) . length . C.output . brain
    isDone = C.isHalt . brain

message :: Robot -> String
message robot = unlines $ map (\y -> map (\x -> translate (x, y) robot) xs) ys
  where
    translate coords = toChar . M.findWithDefault Black coords . panels
    keys = M.keys $ panels robot
    (a, b) = (map fst keys, map snd keys)
    xs = [minimum a .. maximum a]
    ys = reverse [minimum b .. maximum b]
    toChar Black = '.'
    toChar White = '#'

apply :: Instruction -> Robot -> Robot
apply (c, d) = clear . setInput . move . turn (toEnum d) . paint (toEnum c)

paint :: Color -> Robot -> Robot
paint color robot@Robot {position, panels} =
  robot {panels = M.insert position color panels}

turn :: Turn -> Robot -> Robot
turn i robot =
  case (i, orientation robot) of
    (L, o) -> robot {orientation = Cycle.pred o}
    (R, o) -> robot {orientation = Cycle.succ o}

move :: Robot -> Robot
move robot =
  case (position robot, orientation robot) of
    ((i, j), North) -> robot {position = (i, j + 1)}
    ((i, j), South) -> robot {position = (i, j - 1)}
    ((i, j), West) -> robot {position = (i - 1, j)}
    ((i, j), East) -> robot {position = (i + 1, j)}

setInput :: Robot -> Robot
setInput robot@Robot {panels, position, brain} =
  robot {brain = C.setInput [fromEnum color] brain}
  where
    color = M.findWithDefault Black position panels

clear :: Robot -> Robot
clear robot@Robot {brain} = robot {brain = C.clearOutput brain}
