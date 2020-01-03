import Control.Monad
import qualified Data.Map as M
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Nanofactory

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

reduceN :: Int -> Factory -> ([Product], Leftovers)
reduceN i = liftM2 (,) state leftovers . (!! i) . iterate reduce

specs :: Spec
specs = do
  describe "process" processSpecs
  describe "reduce" reduceSpecs
  describe "oresPerFuel" oresPerFuelSpecs
  describe "totalFuel" totalFuelSpecs

processSpecs :: Spec
processSpecs = do
  it "handles missing product" $
    process Nothing (Just (("AB", 1), [("A", 3), ("B", 4)])) `shouldBe` Nothing
  it "handles missing reaction" $
    process (Just ("BC", 3)) Nothing `shouldBe` Just ([("BC", 3)], Nothing)
  it "handles mismatch between product and reaction" $
    process (Just ("BC", 3)) (Just (("AB", 1), [("A", 3), ("B", 4)])) `shouldBe`
    Nothing
  it "handles basic case without leftover" $
    process (Just ("AB", 2)) (Just (("AB", 1), [("A", 3), ("B", 4)])) `shouldBe`
    Just ([("A", 6), ("B", 8)], Nothing)
  it "handles basic case with leftover" $
    process (Just ("AB", 2)) (Just (("AB", 3), [("A", 3), ("B", 4)])) `shouldBe`
    Just ([("A", 3), ("B", 4)], Just ("AB", 1))
  it "handles simple case without leftover" $
    process (Just ("AB", 40)) (Just (("AB", 5), [("A", 3), ("B", 4)])) `shouldBe`
    Just ([("A", 24), ("B", 32)], Nothing)
  it "handles simple case with leftover" $
    process (Just ("AB", 37)) (Just (("AB", 5), [("A", 3), ("B", 4)])) `shouldBe`
    Just ([("A", 24), ("B", 32)], Just ("AB", 3))

reduceSpecs :: Spec
reduceSpecs = do
  it "reduces the basic example once" $
    (reduceN 1 . initialize) basic `shouldBe`
    ([("AB", 2), ("BC", 3), ("CA", 4)], M.empty)
  it "reduces the basic example twice" $
    (reduceN 2 . initialize) basic `shouldBe`
    ([("A", 10), ("B", 23), ("C", 37)], M.empty)
  it "reduces the basic example thrice" $
    (reduceN 3 . initialize) basic `shouldBe`
    ([("ORE", 165)], M.fromList [("B", ("B", 1)), ("C", ("C", 3))])

oresPerFuelSpecs :: Spec
oresPerFuelSpecs = do
  it "works with a basic example" $
    (oresPerFuel . initialize) basic `shouldBe` 165
  it "works with a simple example" $
    (oresPerFuel . initialize) simple `shouldBe` 13312
  it "works with a complex example" $
    (oresPerFuel . initialize) complex `shouldBe` 2210736

totalFuelSpecs :: Spec
totalFuelSpecs = do
  it "works with a simple example" $
    (totalFuel . initialize) simple `shouldBe` 82892753
  it "works with a complex example" $
    (totalFuel . initialize) complex `shouldBe` 460664

basic :: String
basic =
  "9 ORE => 2 A\n\
    \8 ORE => 3 B\n\
    \7 ORE => 5 C\n\
    \3 A, 4 B => 1 AB\n\
    \5 B, 7 C => 1 BC\n\
    \4 C, 1 A => 1 CA\n\
    \2 AB, 3 BC, 4 CA => 1 FUEL"

simple :: String
simple =
  "157 ORE => 5 NZVS\n\
    \165 ORE => 6 DCFZ\n\
    \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
    \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
    \179 ORE => 7 PSHF\n\
    \177 ORE => 5 HKGWZ\n\
    \7 DCFZ, 7 PSHF => 2 XJWVT\n\
    \165 ORE => 2 GPVTF\n\
    \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

complex :: String
complex =
  "171 ORE => 8 CNZTR\n\
    \7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
    \114 ORE => 4 BHXH\n\
    \14 VRPVC => 6 BMBT\n\
    \6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
    \6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
    \15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
    \13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
    \5 BMBT => 4 WPTQ\n\
    \189 ORE => 9 KTJDG\n\
    \1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
    \12 VRPVC, 27 CNZTR => 2 XDBXC\n\
    \15 KTJDG, 12 BHXH => 5 XCVML\n\
    \3 BHXH, 2 VRPVC => 7 MZWV\n\
    \121 ORE => 7 VRPVC\n\
    \7 XCVML => 6 RJRHP\n\
    \5 BHXH, 4 VRPVC => 5 LTCX"
