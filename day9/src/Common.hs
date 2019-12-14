module Common
  ( solve
  , debug
  , Action
  ) where

import Debug.Trace

type Action a = Reduction a -> IO ()

type Path = String

type Problem = (Int, Int)

type Reduction a = String -> a

solve :: Show a => Problem -> Action a
solve day func = apply "input.txt" func >>= print . pretty day

apply :: Path -> Reduction a -> IO a
apply file func = func <$> readFile file

pretty :: Show a => Problem -> a -> String
pretty (a, b) = (++) ("Day " ++ show a ++ "." ++ show b ++ ": ") . show

debug :: Show a => String -> a -> a
debug label x = traceShow (label ++ show x) x
