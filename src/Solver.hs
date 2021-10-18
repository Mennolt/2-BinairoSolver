{-|
   Module       : Solver
   Description  : Types and functions for Binairo solvers
   Copyright    : (c) Tom Verhoeff, 2021
   License      : None

   The "Solver" module defines and exports these things:

   * The data types `M`, `Strategy`, and `MetaStrategy`
   * Various functions to update Binairo puzzles
-}
module Solver (
                Extra
              , M (..)
              , printM
              , Strategy
              , update
              , fillEmpty
              , sE
              , MetaStrategy
              , mP
              , mC
              , findLocs
              , sT
              , mF
              , sFT
              , sL
              , sFPFTL
              , mG
              , mFG
              , sB
              ) where
  import Utilities
  import Binairos
  import Control.Monad
  import Control.Monad.Fix

  -- TODO 5.1: add data type `Change`; its `show`, and `showChanges`

  {-|
    = Extra data type

    Extra result data for monad `M`.
  -}
  data Extra = Extra {
      eChanged :: Bool,
    -- TODO 4.1 and TODO 5.2: add fields
    eChangeCount :: Int 
    }

  {-|
    Function to print extra result data.
  -}
  printE :: Extra -> IO ()
  -- TODO 4.2 and TODO 5.3: also print added fields
  printE (Extra b n) =
    do putStrLn (if b then "changes: " ++ n else "No change")

  {-|
    Function to join extra result data.
  -}
  joinExtra :: Extra -> Extra -> Extra
  -- TODO 4.3 and TODO 5.4: include joining of added fields
  joinExtra (Extra b1 n1) (Extra b2 n2) =
    Extra (b1 || b2) (n1 + n2)

  {-|
    = M data type

    Result monad for strategies, to include extra data alongside a puzzle.
    Actually a writer monad with boolean data on the side channel.
    We use it as `M Puzzle`.
    In this assignment, we explore versions of this monad with additional data.
    This initial version adds a boolean, indicating whether the puzzle was changed.
  -}
  data M a = M { mUnwrap :: a, mExtra :: Extra }
           -- TODO 6.1 and TODO 7.1: extend the monad with shortcut cases

  instance Functor M where
    -- fmap :: (a -> b) -> M a -> M b
    fmap f (M p extra) = M (f p) extra
    -- TODO 6.2 and TODO 7.2: add definition patterns for cases added in `M`

  {-|
    Function to flatten a nested monadic value.
    (It turns out that we use the Any monoid to join booleans.)
    The booleans indicating change propagate via disjunction.
  -}
  flattenM :: M (M a) -> M a
  flattenM (M (M p extra2) extra1) =
    M p (joinExtra extra1 extra2)
  flattenM (M m _) = m
  -- incomplete patterns okay; flattenM is not called for other cases in `M`

  instance Monad M where
    -- return :: a -> M a
    -- TODO 4.4 and TODO 5.5: add units for added fields
    return p = M p (Extra False 1)

    -- (>>=) :: m a -> (a -> m b) -> m b  -- a.k.a. "bind"
    -- TODO 6.3 and TODO 7.3: add definition patterns for cases added in `M`
    m >>= f = flattenM (fmap f m)

  -- ignore this definition
  -- the following is needed to satisfy the compiler, but we won't use it
  -- (a monad is an applicative, and applicative operators can be defined in terms of the monad operators)
  instance Applicative M where
    pure = return
    (<*>) = ap

  {-|
    Function to print monadic puzzle values.
  -}
  printM :: M Puzzle -> IO ()
  printM (M p extra) =
    do printP p
       printE extra
  -- TODO 6.4 and TODO 7.4: add definition patterns for cases added in `M`



  {-|
    Strategy type

    Assumption: a strategy only fills empty cells.
  -}
  type Strategy = Puzzle -> M Puzzle

  {-|
    Empty strategy: does nothing.
  -}
  sE :: Strategy
  sE = return  -- i.e. sE p returns p with False to indicate no change

  {-|
    Function to do lazy update in a single location.
    Does nothing if the cell is not empty.

    Assumptions:
    * `cs /= Empty && cs /= Undefined`
  -}
  -- TODO 5.6: add `note` parameter
  update :: CellState -> Location -> Strategy
  update cs loc p
    | grid p loc /= Empty = return p
    -- TODO 6.5 and 7.5, 8.3, 9.2: add/modify conditions for shortcuts results
    | otherwise = M q (Extra True)  -- TODO 4.5 and 5.6: add extra change data
    where
      n = size p
      gr loc2 = if loc2 == loc then cs else grid p loc2
      q = Puzzle { size = n, grid = gr }  -- TODO 8.3: add field `empty`

  {-|
    MetaStrategy type
  -}
  type MetaStrategy = Strategy -> Strategy

  {-|
    Pair meta-strategy: sequential composition.
    It is associative, and has `sE` as unit.
  -}
  mP :: Strategy -> MetaStrategy
  mP s1 s2 = s1 >=> s2  -- reverse Kleisli composition of the monad M; it accumulates the extra data

  {-|
    Function to compose list of strategies.
  -}
  mC :: [Strategy] -> Strategy
  mC = foldr mP sE
    
  -- "TODO 1.3: Define as foldr, using mP and sE"

  {-|
    Function to fill all empty cells in a given collection of locations.
  -}
  -- TODO 5.7: add `note` parameter
  fillEmpty :: CellState -> [Location] -> Strategy
  fillEmpty c = mC . (map upd)
    where
      upd = update c

  {-|
    Triplet strategy: completes one forced triplet, if available.
  -}
  sT :: Strategy
  sT p = let r = findLocs p (allTripletLocs p) 2
    in if null r then return p
       else let (locs, cs) = head r in fillEmpty (opposite cs) locs p  -- TODO 5.7: add note "sT"

  {-|
    Fixpoint meta-strategy: apply strategy until no change.
  -}
  mF :: MetaStrategy
  mF s p =
    let mq = s p -- apply s once, returning a monadic puzzle value
    in case mq of
      (M _ extra) -> if eChanged extra then mq >>= mF s else mq
      _ -> mq  -- puzzle hasn't changed; this handles any cases added in `M`

  {-|
    Fixpoint of triplet strategy.
  -}
  sFT :: Strategy
  sFT = mF sT

  {-|
    Line strategy: completes one forced line, if available.
  -}
  sL :: Strategy
  sL p = let r = findLocs p (allLineLocs p) (size p `div` 2)
    in if null r then return p
       else let (locs, cs) = head r in fillEmpty (opposite cs) locs p  -- TODO 5.7: add note "sL"

  {-|
    Fixpoint of the pair fixpoint-of-triplet-strategy and line strategy.
  -}
  sFPFTL :: Strategy
  sFPFTL = mF (mP sFT sL)

  {-|
    General contradiction meta-strategy for one location and cell state.
  -}
  mG1 :: CellState -> Location -> MetaStrategy
  mG1 cs loc s p =
    if grid p loc /= Empty then return p
    else
      let mq = update cs loc p >>= s  -- TODO 5.7; add note "mG try"
          q = mUnwrap mq  -- TODO 6.6; remove this and use `case mq of` with patterns instead of `if then else`
      in if isValid q then return p
      else update (opposite cs) loc p  -- TODO 5.7: add note "mG found"
      -- TODO 6.6 and TODO 7.6: handle the appropriate cases `Invalid q` and `Solved q` respectively

  {-|
    General contradiction meta-strategy on all empty locations and cell states.
  -}
  mG :: MetaStrategy
  mG s p = mC [mG1 cs loc s | loc <- emptyLocs p, cs <- [Zero, One]] p
  -- N.B. the call of `mG1 cs loc` may be for a `loc` that is not empty, in spite  of `loc <- emptyLocs p`!
  -- viz. in case of `mG1 One loc`, when the preceding `mG1 Zero loc` already found a contradiction.

  {-|
    Fixpoint of general contradiction meta-strategy.
  -}
  mFG :: MetaStrategy
  mFG = mF . mG

  {-|
    Apply mFG to itself.
    This is like backtracking (but slooooow, unless the monad is improved).
  -}
  sB :: Strategy
  sB = fix mFG
