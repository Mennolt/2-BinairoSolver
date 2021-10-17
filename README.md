# Assignment 2-BinairoSolver

## Introduction

The main learning goals of this assignment are:
* to understand _monads_ better (monads were treated in Lecture 9);
* to get some experience with _lazy updaters_ for _immutable_ data types.

This assignment concerns the automated solving of _simplified_ [Binairo puzzles](https://en.wikipedia.org/wiki/Takuzu).
Most Haskell code to define and manipulate such puzzles is already given.
In particular, it involves one _monad_ to add a 'side channel' with extra data
when composing so-called _reasoning strategies_ that transform puzzles
(functions with a puzzle as input and as output).
The monad's _bind_ operator handles the extra result data under composition.

Your challenge is to explore various extensions of this monad to add functionality and
to improve efficiency of the solver.

The article [Look Ma: Backtracking without Recursion](v15_2021_119_132.pdf) (PDF) explains
the simplified Binairos,
and an approach to solving these based on reasoning strategies without backtracking (read §2).
The following two section summarize this.

First, we define the simplified Binairo puzzles.
Next, we describe what strategies and meta-strategies are.
After that, there is a sequence of exercises to go though.

## Simplified Binairo Puzzles

A simplified Binairo puzzle consists of a square grid
with an _even_ number of rows and columns,
partly filled with zeroes and ones.
The code contains two examples.

The objective is to fill the grid completely with zeroes and ones,
such that
* (Rule 1) all _triplets_
  (three horizontally or vertically adjacent cells)
  contain _both_ symbols, and
* (Rule 2) in each line (row and column), the number
  of zeroes _equals_ the number of ones.

The simplification is that we allow identical rows and columns.
The given zeroes and ones cannot be changed when solving
the puzzle, and you may assume that there is a _unique_ solution.

## Strategies and Metastrategies

A (reasoning) _strategy_ is a function that takes a puzzle as input and produces a
-- possibly updated -- puzzle as output,
where the update is a logical consequence of the Binairo rules.
Since data is _immutable_, we use a _lazy_ approach to updats.
The approach in this assignment is not the most efficient,
since it involves linear chains of updates,
rather than [_trees with zippers_](http://learnyouahaskell.com/zippers).
But it will give you an idea of how this could be better than copying all data except for the small change.

To add functionality and improve efficiency, strategy functions can return additional data.
But we would like to compose them as if they just returned an updated puzzle.
One example of extra data is a boolean that indicates whether the puzzle was actually changed by the strategy.
That way, the client code invoking the strategy does not have to compare the input and output puzzle
to determine whether there was a change.
The monad with this extra boolean is present in the given Haskell code.

Basic strategies are:
* `sE` which does nothing (return puzzle unchanged).

  The name `sE` stands for `strategy Empty`.
* `sT` which looks for a _triplet_ (3 adjacent cells),
  one of which is empty and the other two with the same bit `b`,
  in which case the empty cell must contain `1 - b`.

  The name `sT` stands for _strategy Triplet_.
* `sL` which looks for a _line_ (a complete row or column of cells),
  half of them filled with bit `b` and at least one of them empty,
  in which case all empty cells must contain `1 - b`.

  The name `sL` stands for _strategy Line_.

A _meta-strategy_ is a function that takes one or more strategies as parameters
and returns a strategy.
Some basic metastrategies are:
* `mP s t` which returns a strategy that,
  when applied to puzzle `p`,
  first applies `s` to `p`, and then applies `t` to the result.
  Thus, this is just function composition applied to the two strategies.
  But because of the monadic result (carrying extra data),
  we cannot use regular function composition,
  but rather use _Kleisli composition_.

  The name `mP` stands for _meta-strategy Pair_.
* `mC strats` which takes a list of strategies `strats` as argument,
  and returns their composition.

  The name `mC` stands for _meta-strategy Compose all_.
* `mF s` which keeps on applying s until there was no change.

  The name `mF` stands for _meta-strategy Fixpoint_,
  since, by definition, `s` applied to `mF s` is the same as `mF s`.

The extra boolean result mentioned above
is especially useful for `mPf` and `mF`.
It is also useful to generalize `mP` and `mPf` to lists of strategies,
using a fold.

An idea for a more advanced meta-strategy is
* `mG s` which goes over all empty cells,
  and tries successively a 0 and a 1 there,
  in each case applying strategy `s` afterwards
  (to do 'forced' updates)
  and if there was a rule violation (contradiction),
  updates the puzzle with the opposite bit.
  If no contradiction arises,
  then the puzzle is returned unchanged.

  The name `mG` stands for _meta-strategy General contradiction_
  (general, because you can still choose `s`;
  often, you see that people use it with `sL` for `s`).

It turns out that `mG sE` behaves like a combination of
`sT` and `sL` (though it is a bit less efficient).
The more powerful the strategy `s` in `mG s` is,
the better `mG s` works.

Let `mFG s` be `mF (mG s)`, repeating `mG s` until no change.
Then applying `mFG` _with itself as parameter_
seems to be the most powerful strategy imaginable.
In Haskell, you cannot type a function with self-application
(it gives rise to an infinite type).
But there is a way to accomplish this indirectly
through `sB = fix mFG`.
Here, the name `sB` stands for _strategy Backtrack_,
because it is basically backtracking.

There is, however, a snag.
Isn't there always when things look too good to be true?
`sB` does work, and it solves any puzzle with a unique solution.
But it isn't efficient (so, don't try it on the example puzzles just yet):
* When `mG s` applies `s` and this leads to an intermediate result with a contradiction,
  this contradiction is ignored until `s` completes.

  It would be better if strategies could return as extra data whether
  a contradiction occurred (like an exception).
  This then can shortcut all subsequent strategy applications
  (those other strategies are irrelevant because they can't undo that contradiction).
* Moreover, when applying `s` leads to an intermediate result that actually solves the puzzle,
  this is also ignored by `mG` (since it is not a contradiction).
  `sB` can happily conclude that a 0 does not lead to a contradiction
  (but to an unnoticed solution),
  then try a 1 (which does lead to a contradiction if the puzzle has a unique solution),
  and only then it will decide to update the puzzle with that 0 after all.

  It would be better if strategies could return as extra result
  whether a solution was found (like an exception).
  This then can shortcut all subsequent strategy applications.

Handling such extra data appropriately under composition is exactly what
monads are for.

## Exercises

Places where you (may) need to do some work in the source code
are marked with `TODO`
(for Exercise 1 appearing inside the message of an `error` call).

Your answers for Exercise `i` (except Exercise 1) must written
in text file `answers/answer-exercise-i.txt`.
A [template file](answers/answer-exercise-template.txt) is given.
Personalize this template file first,
and then _copy_ and _rename_ it for each exercise.

### Scoring

For each successfully completed exercise you get 1 point.

### Exercise 1 - Complete the folds

There are three uses of `foldr` that are incomplete.
Complete those definitions to obtain code that can run:
1. In [`Utilies.hs`](src/Utilities.hs), function `joinStr`
2. In [`Binairos.hs`](src/Binairos.hs), function `countCS`
3. In [`Solver.hs`](src/Solver.hs), function `sC`

There are test cases for `joinStr` and `countCS`,
invoked by `cabal test`.

### Exercise 2 - Run some examples

Run the following commands, and collect each command and its output
in the text file `answers/answer-exercise-2.txt`.
Also include the author names, their TU/e ID number,
and the date, and commit this text file to the root of your repository.

* `printPR example6PR`
* `printPR example8PR`
* `printP example6`
* `printP example8`
* `printM $ sE example8` (`printM` prints monadic puzzle values,
  showing the extra data)
* `printM $ sT example8`
* `example8FT = mF sT example8`

  `printM $ example8FT`

  `printM $ mF sT example8FT`
  
  Note that the second application of `mF sT` reports no change.
  (Strategy `mF sT` has abbreviation `sFT`)
* `printM $ mP sFT sL example8` (This should have filled one more cell.)
* `printM $ sFPFTL example8` (Look up the definition of `sFPFTL`;
  this should have filled two further cells.)
* `printM $ mF (mG sE) example8` (This fills the same cells as the preceding;
  `mF (mG s)` has abbreviation `mFG`.)
* `printM $ mG sL example8` (This fills more cells than the preceding.)
* `printM $ mFG sL example8` (This solves the puzzle; be patient)

(Don't try `sB` yet because it will be too slow.)

### Exercise 3 - Explain performance

1. Explain why the following functions are not as inefficient
   as they may seem to be at first sight:
   * `sT` and `sL`, which call `findLocs` that returns _all_ triplets/lines
     with an empty cell and 'enough' other cells having the same bit,
     but only uses the _first_ such triplet/line (if it exists).
2. Explain why the 'lazy' approach to updates in this assignment 
   is still inefficient.
   How would it compare to doing a full copy on each update?
3. Explain why `solution = sB example8` terminates instantly,
   whereas `printM $ sB example8` does not seem to terminate.
   (You can interrupt execution with Ctrl-C;
   we will improve the performance of `sB` in the exercises below.)
   Hint: Try `printM solution`.

Write your explanations in a text file `answers/answer-exercise-3.txt`.

### Exercise 4 - Extend monad `M` with a change count

The extra data (cf. `Extra` and monad `M`),
viz. a single boolean, is enough to make
the fixpoint meta-strategy `mF s` more efficient.
Without that boolean, it would need to compare the entire puzzle `p`
with the result of `s p` to find out whether there was a change.

However, this boolean is not very informative for us, as you have seen in Exercise 2.
It would have been nice if the _number of changes_ would be reported.
We could then remove the boolean, since it can be inferred from that number.
But we will keep it to avoid having to change code that uses it.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
1. Adapt `data Extra`: add a field to the 'record' (you can guess the type).
   Suggested name for its selector: `eChangeCount`.
2. Adapt `printE`: also print the change count, e.g. as `# changes: <n>`.
3. Adapt `joinExtra`: incorporate how to accumlate the change counts.
4. In `M`, adapt its `return` function,
   to produce the unit element for counting.
5. Adapt `update`, to report the change count.

N.B. There is no need (yet) to change `flattenM`, `>>=`, `printM`,
or any other code for that matter.

Go over the commands in Exercise 2 again, and collect the new output
as evidence in text file `answers/answer-exercise-4.txt`.

### Exercise 5 - Extend monad `M` with log of change details

It would be even more informative
if strategies could produce a log detailing which cells where changed when
and for what reason.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
1. Add `data Change = Change {cLoc :: Location, cCS :: CellState, cNote :: String}`
   to capture the location, new state, and a note for a single change.
   We will use `[Change]` for the log.
   * Make `Change` and instance of `Show` and define `show` for a change,
     e.g., producing `(<i>, <j>) -> <cs>  <note>`.
   * Define function `showChanges` to convert a list of changes to a string,
     one change per line (you can use `joinStr` from `Utilies` here).
2. Adapt `data Extra`: add a field to the 'record'.
   Suggested name for its selector: `eChangeLog` of type `[Change]`.
3. Adapt `printE`: also print the change log, using `showChanges`.
4. Adapt `joinExtra`: incorporate how to accumlate the change logs.
  
   N.B. For efficiency reasons, it is best to accumulate changes in _reverse_
   chronological order. (Why?)
5. In `M`, adapt its `return` function:
   produce the unit element for logging.
6. Adapt `update`: report the change.
   In order to include a note,
   add `note` as first parameter to `update`,
   and use that as `cNote` in the change.
   Keep in mind that this change must have type `[Change]`.
7. All calls of `update` must supply that extra `note` parameter:
   * `fillEmpty`: also add a `note` parameter here,
     and adapt the calls of `fillEmpty`.
   * `mG1` (twice)

Go over the commands in Exercise 2 again, and collect the new output
as evidence in text file `answers/answer-exercise-5.txt`.

### Exercise 6 - Stop solver early at a contradiction

When applying `mFG` to itself, it can get sidetracked
into finding a contradiction, while finding a contradiction,
while finding ...
And that, while all the time there already was a contradiction.

By extending monad `M` with a shortcut case for contradictions,
we can bypass all remaining strategies whenever a contradiction
was encountered.

In order to allow monadic composition to bypass subsequent functions,
monad `M` must have a case that does not involve the type `a` that it 'decorates'.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
1. Adapt monad `M`: add an extra case
   `| Invalid {iPuzzle :: Puzzle}`.
   The `iPuzzle` field is used to report the invalid puzzle
   (which can be useful for debugging).

   Note that the case `M p extra` can now be viewed as a
   puzzle that is _valid_.
2. In `instance Functor M`, adapt `fmap`: add
   definition for pattern `fmap (Invalid p)`,
   doing nothing.
3. In `instance Monad M`, adapt `>>=`:
   add definition for pattern `Invalid p >>= _`,
   ignoring and not evaluating `f`, i.e., the result is `Invalid p`.
   Place this definition _above_ the definition
   `m >>= f = flattenM (fmap f m)`,
   which then serves as the 'otherwise' situation.
4. Adapt `printM`: add definition for pattern
   `printM (Invalid p)`.
   Print `INVALID` above the reported puzzle `p`.
5. Adapt `update`: return `Invalid q`, when the updated puzzle `q`
   is invalid (`not (isValid q)`).
   Note that this check is expensive and can be improved
   (see Exercise 9).
6. Adapt `mG1`:
   use the result `Invalid q` to detect a contradiction,
   rather than calling `isValid`.
   You can remove `q = mUnwrap mq`,
   if you use `case mq of` with patterns `Invalid q ->`
   and `_ ->` instead of `if then else`.

Run the following commands and collect them with their output
in the text file `answers/answer-exercise-6.txt`.
* `printM $ sB example6` (be patient; this may take a few minutes)

Strategy `sB` is still too slow for `example8`.

### Exercise 7 - Stop solver early at a solution

In order to allow monadic composition to bypass subsequent functions,
monad `M` must have a case that does not involve the type `a` that it 'decorates'.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
1. Adapt monad `M`: add an extra case
   `| Solved {sPuzzle :: Puzzle}`.
   The `sPuzzle` field is used to report the solution found.

   Note that the case `M p extra` can now be viewed as a
   puzzle that is _valid_ and _unsolved_.
2. In `instance Functor M`, adapt `fmap`: add
   definition for pattern `fmap (Solved p)`,
   doing nothing.
3. In `instance Monad M`, adapt `>>=`:
   add definition for pattern `Solved p >>= _`,
   ignoring and not evaluating `f` (like for `Invalid p`).
   Place this definition _above_ the current definition
   `m >>= f = flattenM (fmap f m)`,
   which then serves as the 'otherwise' situation.
4. Adapt `printM`: add a definition for the pattern
   `printM (Solved p)`.
   Print `Solution` above the reported solution `p`.
5. Adapt `update`: return `Solved q`, when the updated puzzle `q`
   is a solution (`isSolved q`).
   Place this condition _below_ the condition `isValid q`,
   because then `not (hasEmpty p)` suffices to check for a solution.
6. Adapt `mG1`: in `case mq of`,
   use the result `Solved q` to detect a solution,
   and pass it on: `Solved q -> mq`.

Note that this way of stopping early
has a price, viz. that at every update,
the puzzle is checked for being solved.
And `hasEmpty` is still expensive (we address that in Exercise 8).
But now `sB` will be quite usable.

Run the following commands and collect them with their output
in the text file `answers/answer-exercise-7.txt`.
* `printM $ sB example6`
* `printM $ sB example8` (should not take more than a few seconds)

### Exercise 8 - Detect solutions more efficiently

To check whether the puzzle is solved,
knowing that it is valid,
it suffices to know how many empty cells there are:
no empty cells corresponds to solved.
Let us extend a puzzle to keep track of
the _number of empty cells_ as it changes.

These are the changes to make in [`Binairos.hs`](src/Binairos.hs):
1. Adapt `data Puzzle`: add field `empty` (you know the type).
2. Adapt `fromPR`: calculate and add the field `empty` in the result.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
3. Adapt `update`: use field `empty` to detect a solution,
   instead of using `isSolved`.

Run the following commands and collect them with their output
in the text file `answers/answer-exercise-8.txt`.
* `printM $ sB example6`
* `printM $ sB example8` (should be faster than in Exercise 7)

### Exercise 9 - Detect contradictions more efficiently

Rather than calling `isValid` in `update`,
we could check only the triplets and lines containing the updated cell
(because we know that the given puzzle was valid).
There are at most 6 such triplets and exactly 2 such lines.

These are the changes to make in [`Binairos.hs`](src/Binairos.hs):
1. Add these functions
   * `cellTripletLocs :: Location -> Puzzle -> [[Location]]`:
     generate the (max. 6) triplets for given location.
     You could just generate all 6, possibly with out-of-range locations.
     That is fine, because the puzzle grid function returns `Undefined`
     for out-of-range locations (cf. `fromPR`).
   * `cellLineLocs :: Location -> Puzzle -> [[Location]]`:
     generate the 2 lines for given location.
   * `isValidLoc :: Location -> Puzzle -> [[Location]]`:
     check validity of all triples and lines for given location.
   
   Don't forget to add these functions the export list of the module.

These are the changes to make in [`Solver.hs`](src/Solver.hs):
2. Adapt `update`: use `isValidLoc` instead of `isValid`.

Run the following commands and collect them with their output
in the text file `answers/answer-exercise-9.txt`.
* `printM $ sB example6`
* `printM $ sB example8` (should be fast now)

### Exercise 10 - More enhancements

There are various ways to enhance the solver.
Also see [Look Ma: Backtracking without Recursion](v15_2021_119_132.pdf).
We give several suggestions.
These are (conceptually) (considerably) harder.
So, feel free to skip this.

Provide evidence in text file `answers/answer-exercise-10.txt`.

#### Transfer change log to `Invalid` and `Solved`

It is a pity that the change log gets lost when
a contradiction of solution are found.

Things to do in [`Solver.hs`](src/Solver.hs):
1. Add `iChangeLog` and `sChangeLog` fields to `Invalid` and `Solved`.
2. Adapt `flattenM` and `>>=` to do the transfer.

#### Also report the log in `Invalid` and `Solved`

So far, monad `M` is -- what is known as -- a _Writer_ monad.
Recall that strategies have type `Puzzle -> M Puzzle`.
The extra information produced by the preceding strategy
is not available inside the next strategy
(such as inside `update` and the strategies).
It is handled completely outside by the monadic composition
(viz. `>>=` and `>=>`).

To get the ability to use that information,
we need a [_State_ monad](http://learnyouahaskell.com/for-a-few-monads-more#state).
A State monad has the shape `data M a = M (State -> (a, State))`.
You also need to get the (shortcut) cases into this monad.

#### Add a level parameter for general contradiction

This level can then be used to indent the change log accordingly,
to visualize the recursive structure of backtracking.
Again, this needs a `State` monad, to carry the current level along.

#### Add a parameter to stop the solver at the first change

This is similar adding a level parameter,
except that a [_Reader_ monad](http://learnyouahaskell.com/for-a-few-monads-more#reader) would suffice
(this is a special case of the _State_ monad: `data M a = M (Env -> a)`).
However, now the monadic composition must depend on this parameter.


## (End of Assignment 2)
