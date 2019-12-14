module Common
  ( solve
  , Action
  ) where

type Action a = Reduction a -> IO ()

type Path = String

type Problem = (Int, Int)

type Reduction a = String -> a

solve :: Problem -> Action String
solve day func = apply "input.txt" func >>= putStrLn . pretty day

apply :: Path -> Reduction String -> IO String
apply file func = func <$> readFile file

pretty :: Problem -> String -> String
pretty (a, b) = (++) ("Day " ++ show a ++ "." ++ show b ++ ": ")
