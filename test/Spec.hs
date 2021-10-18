{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Utilities
import Binairos
import Solver
import Control.Monad

-- A QuickCheck generator for strings of length 0 through 4.
strings :: Gen String
strings = let xs = ["", "a", "ab", "abc", "abcd", "abcde"]
  in do i <- choose (0, length xs-1)
        return (xs!!i)

-- generate lists of cell states of various lengths
-- all cell states in a list
allCS :: [CellState]
allCS = [Empty, Zero, One, Undefined]

-- construct all lists of cell states of given length
allCSs :: Int -> [[CellState]]
allCSs 0 = [[]]
allCSs n = [c : xs | c <- allCS, xs <- allCSs (n-1)]

-- construct all lists of cell states with length in given interval
allCSsBetween :: Int -> Int -> [[CellState]]
allCSsBetween m n = [xs | i <- [m .. n], xs <- allCSs i]

-- generatel cell states with length in given interval
cellstatelists :: Int -> Int -> Gen [CellState]
cellstatelists m n = let css = allCSsBetween m n
  in do i <- choose (0, length css-1)
        return (css!!i)

-- check counts of cell states
checkCScounts :: [CellState] -> (Int, Int, Int, Int) -> Bool
checkCScounts cs (cE, c0, c1, cU) = [cE, c0, c1, cU] == map count allCS where
  count c = length . filter (== c) $ cs

-- an empty 2x2 puzzle
p2 :: Puzzle
p2 = fromPR ["..",".."]

-- update location (0, 0) in given puzzle to cell state given as character
-- assumption: cs in ".01#"
upd00 :: Char -> Strategy
upd00 cn p =
  let (_ : cs) : rs = toPR p
  in return (fromPR ((cn : cs) : rs))

-- update location (0, 1) in given puzzle to cell state given as character
-- assumption: cs in ".01#"
upd01 :: Char -> Strategy
upd01 cn p =
  let (c : _ : cs) : rs = toPR p
  in return (fromPR ((c : cn : cs) : rs))

fromMtoPR :: M Puzzle -> PuzzleRep
fromMtoPR mp = (toPR . mUnwrap) mp

main :: IO ()
main = hspec $ do
  describe "Assignment 2 - Binairo Puzzles" $ do

    describe "Utilities: joinStr \"\"" $ do

      it "has right inverse `splitStr`" $ property $
        forAll strings (\s -> joinStr "" "" (splitStr s) == s)

      it "has `filter` as left inverse" $ property $
        forAll strings (\s -> (filter (flip notElem "+.") . joinStr "+" "." . splitStr) s == s)

    describe "Binairos: countCS" $ do

      it "counts CellStates in list of CellStates upto length 3" $ property $
        forAll (cellstatelists 0 3) (\css -> checkCScounts css (countCS css))

      it "counts CellStates in list of CellStates of length 5" $ property $
        forAll (cellstatelists 5 5) (\css -> checkCScounts css (countCS css))

    describe "Solver: mC" $ do

      it "should compose [] into doing nothing" $ do
        (fromMtoPR . mC []) p2 `shouldBe` toPR p2

      it "should compose [s] into s" $ do
        (fromMtoPR . mC [upd00 '0']) p2 `shouldBe` fromMtoPR (upd00 '0' p2)

      it "should compose [s, t] into first do s, then do t" $ do
        (fromMtoPR . mC [upd00 '0', upd00 '1']) p2 `shouldBe` fromMtoPR (upd00 '0' p2 >>= upd00 '1')

      it "should compose [s, t, u] into first do s, then do t, then do u" $ do
        (fromMtoPR . mC [upd00 '0', upd01 '1', upd00 '#']) p2 `shouldBe`
          fromMtoPR (upd00 '0' p2 >>= upd01 '1' >>= upd00 '#')
