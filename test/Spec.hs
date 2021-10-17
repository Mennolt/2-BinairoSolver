{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Utilities
import Binairos
import Solver

-- A QuickCheck generator for strings of length 0 through 4.
strings :: Gen String
strings = let xs = ["", "a", "ab", "abc", "abcd", "abcde"]
  in do i <- choose (0, length xs-1)
        return (xs!!i)

cellstatelists :: Gen [CellState]
cellstatelists = let css =  map (map (\ch -> read ch :: CellState)) $ map splitStr ["", ".", "0", "1", "#", "..", "...", "00", "000", "##", "###", ".01#"]
  in do i <- choose (0, length css-1)
        return (css!!i)

main :: IO ()
main = hspec $ do
  describe "Assignment 2 - Binairo Puzzles" $ do

    describe "Utilities: joinStr \"\"" $ do

      it "has right inverse `splitStr`" $ property $
        forAll strings (\s -> joinStr "" "" (splitStr s) == s)

      it "has `filter` as left inverse" $ property $
        forAll strings (\s -> (filter (flip notElem "+.") . joinStr "+" "." . splitStr) s == s)

    describe "Binairos: countCS" $ do

      it "counts CellStates in a list of CellStates" $ property $
        forAll cellstatelists
          (\css -> let (cE, c0, c1, cU) = countCS css
                   in cE == (length . filter (== Empty)) css &&
                      c0 == (length . filter (== Zero)) css &&
                      c1 == (length . filter (== One)) css &&
                      cU == (length . filter (== Undefined)) css
          )

