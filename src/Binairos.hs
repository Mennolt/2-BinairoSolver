{-|
   Module       : Binairos
   Description  : Data types and functions for Binairo puzzles
   Copyright    : (c) Tom Verhoeff, 2021
   License      : None

   The "Binairo" module defines and exports these things:

   * The data types `PuzzleRep`, `CellState`, `Location`, and `Puzzle`
   * The example puzzles `example6PR`, `example8PR`, `example6`, `example8`
   * Various functions
-}
module Binairos (
                      PuzzleRep
                    , showPR
                    , printPR
                    , example6PR
                    , example8PR
                    , CellState (..)
                    , opposite
                    , countCS
                    , Location
                    , Puzzle (..)
                    , toPR
                    , fromPR
                    , printP
                    , example6
                    , example8
                    , allLocs
                    , allTripletLocs
                    , cellTripletLocs
                    , allLineLocs
                    , cellLineLocs
                    , isValidTriplet
                    , isValidLine
                    , isValid
                    , isValidLoc
                    , emptyLocs
                    , hasEmpty
                    , isSolved
                    , findLocs
                    ) where

  import Utilities
  import Control.Monad ()

  {-|
    = PuzzleRep data type

    A simple human-readable and writable representation for Binairo puzzles,
    viz. as a list of strings, where each character represents a cell.
    The list and the strings in it all have the same even length.
  -}
  type PuzzleRep = [String]

  {-|
    Convert puzzle representation to string.
  -}
  showPR :: PuzzleRep -> String
  showPR p = [ ch | row <- p, ch <- joinStr " " "\n" (splitStr row) ]

  {-|
    Print puzzle representation.
  -}
  printPR :: PuzzleRep -> IO ()
  printPR = putStr . showPR

  {-|
    Example 6x6 puzzle.
  -}
  example6PR :: PuzzleRep
  example6PR = [
    "...1..",
    "..0..1",
    "0...0.",
    ".11...",
    "......",
    "1...0."
    ]

  {-|
    Example 8x8 puzzle.
  -}
  example8PR :: PuzzleRep
  example8PR = [
    "0.0.1...",
    "........",
    ".....11.",
    "00..1...",
    "0.....10",
    "...1....",
    "....1...",
    "...1...0"
    ]

  {-|
    = CellState data type

    Defines the four states of a cell in the puzzle's grid.
  -}
  data CellState = Empty | Zero | One | Undefined
    deriving (Eq)

  instance Show CellState where
    show Empty = "."
    show Zero = "0"
    show One = "1"
    show Undefined = "#"

  instance Read CellState where
    readsPrec _ (c : rest)
      | c == '.' = [(Empty, rest)]
      | c == '0' = [(Zero, rest)]
      | c == '1' = [(One, rest)]
      | c == '#' = [(Undefined, rest)]
    readsPrec _ _ = []  -- no parse

  {-|
    Function to determine opposite state, where `Zero` <-> `One`.
  -}
  opposite :: CellState -> CellState
  opposite Zero = One
  opposite One = Zero
  opposite cs = cs

  {-|
    Function to count how often each cell state occurs in a list.
  -}
  countCS :: [CellState] -> (Int, Int, Int, Int)
  countCS = foldr upd (0, 0, 0, 0) where
    upd Empty (cE, c0, c1, cU) = (cE + 1, c0, c1, cU)
    upd Zero (cE, c0, c1, cU) = (cE, c0 + 1, c1, cU)
    upd One (cE, c0, c1, cU) = (cE, c0, c1 + 1, cU)
    upd Undefined (cE, c0, c1, cU) = (cE, c0, c1, cU + 1)

  {-|
    = Location data type

    A pair of integer coordinates.
  -}
  type Location = (Int, Int)

  {-|
    = Puzzle data type

    A size, and a grid function to map locations to cell states
  -}
  data Puzzle = Puzzle {
      size :: Int
    , grid :: Location -> CellState
    , empty :: Int
    }

  {-|
    Function to convert `Puzzle` to `PuzzleRep`.
  -}
  toPR :: Puzzle -> PuzzleRep
  toPR puzzle = [ [ch | j <- coord, ch <- show (grid puzzle (i, j)) ] | i <- coord ]
    where coord = [0 .. (size puzzle) - 1]

  {-|
    Function to convert `PuzzleRep` to `Puzzle`.
  -}
  fromPR :: PuzzleRep -> Puzzle
  fromPR pr = Puzzle { size = n, grid = gr, empty = e } where  -- TODO 8.2: add field `empty`, and its calculation
    n = length pr
    gr (i, j)
      | 0 <= i && i < n && 0 <= j && j < n = read [pr !! i !! j]
      | otherwise = Undefined
    e = length $ filter (== '.') (showPR pr)

  {-|
    Function to make one long string out of a `PuzzleRep`
  -}
  --PRtoString :: PuzzleRep -> String
  --PRtoString (prHead : prTail) = prHead ++ PRtoString prTail
  --PRtoString [] = ""

  {-|
    Function to print a puzzle.
  -}
  printP :: Puzzle -> IO ()
  --printP = printPR . toPR 
  printP p = printPR  (toPR p ++ [show (empty p)])

  {-|
    Example 6x6 puzzle in internal format.
  -}
  example6 :: Puzzle
  example6 = fromPR example6PR

  {-|
    Example 8x8 puzzle in internal format.
  -}
  example8 :: Puzzle
  example8 = fromPR example8PR

  {-|
    Function to get list of all locations of puzzle.
  -}
  allLocs :: Puzzle -> [Location]
  allLocs p = let n1 = size p - 1
    in [ (i, j)
       | i <- [0 .. n1]
       , j <- [0 .. n1]
       ]

  {-|
    Function to get list of all triplet locations of puzzle.
  -}
  allTripletLocs :: Puzzle -> [[Location]]
  allTripletLocs p = let n = size p
    in [ [(i + k * m, j + l * m) | m <- [0..2]]
       | (k, l) <- [(0, 1), (1, 0)]
       , i <- [0 .. n - 3 * k]
       , j <- [0 .. n - 3 * l]
       ]

  {-|
    Function to get list of all (max. 6) triplets containing a given location.
  -}
  cellTripletLocs :: Location -> Puzzle -> [[Location]]
  cellTripletLocs (i, j) p = error "TODO 9"

  {-|
    Function to get list of all line locations of puzzle.
  -}
  allLineLocs :: Puzzle -> [[Location]]
  allLineLocs p =  let n1 = size p - 1
    in [ [(i + vert * m, j + hor * m) | m <- [0..n1]]
       | (hor, vert) <- [(1, 0), (0, 1)]
       , i <- [0 .. if hor == 1 then n1 else 0]
       , j <- [0 .. if vert == 1 then n1 else 0]
       ]

  {-|
    Function to get list of all (= both) lines containing a given location.
  -}
  cellLineLocs :: Location -> Puzzle -> [[Location]]
  cellLineLocs (i, j) p = error "TODO 9"

  {-|
    Function to check whether a triplet is valid.

    Assumption: `locs` is a triplet
  -}
  isValidTriplet :: Puzzle -> [Location] -> Bool
  isValidTriplet p locs =
    let (_, c0, c1, _) = countCS (map (grid p) locs)
    in c0 <= 2 && c1 <= 2

  {-|
    Function to check whether a line is valid.

    Assumption: `locs` is a line
  -}
  isValidLine :: Puzzle -> [Location] -> Bool
  isValidLine p locs =
    let (_, c0, c1, _) = countCS (map (grid p) locs)
        n2 = size p `div` 2
    in c0 <= n2 && c1 <= n2

  {-|
    Function to check whether a puzzle is valid.
  -}
  isValid :: Puzzle -> Bool
  isValid p =
    all (isValidTriplet p) (allTripletLocs p) &&
    all (isValidLine p) (allLineLocs p)

  {-|
    Function to check validity involving a single cell.
  -}
  isValidLoc :: Location -> Puzzle -> Bool
  isValidLoc loc p = error "TODO 9"

  {-|
    Function to get locations of all empty cells.
  -}
  emptyLocs :: Puzzle -> [Location]
  emptyLocs p = filter condition (allLocs p) where
    condition loc = grid p loc == Empty

  {-|
    Function to check whether puzzle has empty cells.
  -}
  hasEmpty :: Puzzle -> Bool
  hasEmpty = not . null . emptyLocs

  {-|
    Function to check whether puzzle is solved.
  -}
  isSolved :: Puzzle -> Bool
  isSolved p = isValid p && not (hasEmpty p)

  {-|
    Function to select location lists with cell state from given list of lists of locations,
    such that each returned pair of location list and cell state

    * contains an empty cell in the location list, and
    * contains a cell with a state whose count equals target count

    Returns the list of all result pairs.
  -}
  findLocs :: Puzzle -> [[Location]] -> Int -> [([Location], CellState)]
  findLocs p locss c = filter condition (map f locss) where
    condition :: ([Location], CellState) -> Bool
    condition (_, cs) = cs /= Undefined

    -- f pairs the location list with cell state Zero/One when that occurs c times,
    -- and there is at least one empty cell; otherwise with Undefined
    f :: [Location] -> ([Location], CellState)
    f locs = (locs, cs) where
        (cE, c0, c1, _) = countCS (map (grid p) locs)
        cs | cE == 0 = Undefined
           | c0 == c = Zero
           | c1 == c = One
           | otherwise = Undefined
