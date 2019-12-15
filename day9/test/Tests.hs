import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Computer

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

shouldOutput :: Computer -> [Int] -> Spec
shouldOutput comp xs =
  it (show . state $ comp) $
  (reverse . output . run (not . isHalt) $ comp) `shouldBe` xs

testQuine :: [Int] -> Spec
testQuine xs = build xs `shouldOutput` xs

specs :: Spec
specs = do
  describe "op = 4" $ build [109, -1, 4, 1, 99] `shouldOutput` [-1]
  describe "mode = 1, op = 4" $ build [109, -1, 104, 1, 99] `shouldOutput` [1]
  describe "mode = 2, op = 4" $ do
    build [109, -1, 204, 1, 99] `shouldOutput` [109]
    build [109, 1, 9, 2, 204, -6, 99] `shouldOutput` [204]
    build [109, 1, 109, 9, 204, -6, 99] `shouldOutput` [204]
    build [109, 1, 209, -1, 204, -106, 99] `shouldOutput` [204]
  describe "op = 3" $ do
    (setInput [1] . build) [109, 1, 3, 3, 204, 2, 99] `shouldOutput` [1]
    (setInput [2] . build) [109, 1, 3, 3, 204, 2, 99] `shouldOutput` [2]
    (setInput [100] . build) [109, 1, 3, 3, 204, 2, 99] `shouldOutput` [100]
    (setInput [-1] . build) [109, 1, 3, 3, 204, 2, 99] `shouldOutput` [-1]
  describe "mode = 2, op = 3" $ do
    (setInput [1] . build) [109, 1, 203, 2, 204, 2, 99] `shouldOutput` [1]
    (setInput [2] . build) [109, 1, 203, 2, 204, 2, 99] `shouldOutput` [2]
    (setInput [100] . build) [109, 1, 203, 2, 204, 2, 99] `shouldOutput` [100]
    (setInput [-1] . build) [109, 1, 203, 2, 204, 2, 99] `shouldOutput` [-1]
  describe "quines" $ do
    testQuine
      [101, -5, 5, 5, 4, 5, 101, 6, 5, 5, 1007, 5, 23, 15, 1105, 1, 0, 99]
    testQuine
      [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
