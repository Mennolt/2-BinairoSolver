# Assignment 2-BinairoSolver

## Introduction

This assignment concerns the automated solving of simplified [Binairo puzzles](https://en.wikipedia.org/wiki/Takuzu).
Some Haskell code to define and manipulate such puzzles is already given.
Your challenge is to explore various monads to compose reasoning strategies,
so that their extra result data is handled properly under composition.

The article [Look Ma: Backtracking without Recursion](v15_2021_119_132.pdf) (PDF) explains an approach to solving
simplified Binairos based on reasoning strategies without backtracking (read §2).


## Strategies and Metastrategies

A (reasoning) _strategy_ is a function that takes a puzzle as input and produces a,
possibly updated, puzzle as output,
where the update is a logical consequence of the Binairo rules.
Since data is _immutable_, we use a __lazy update approach.
The approach in this assignment is not the most efficient,
since it uses linear chains of updates,
rather than _trees with zippers_.
But it will give you an idea of how this is better than copying all data except for the small change.

To improve efficiency, strategy functions can return additional data.
But we would like to compose them as if they just returned an updated puzzle.
One example of extra data is a boolean that indicates whether the puzzle was actually changed by the strategy.
That way, the client code invoking the strategy does not have to compare the input and output puzzle
to determine whether there was a change.

Basic strategies are:
* `sE(p)` which does nothing (return `p` unchanged).

  The name `sE` stands for `strategy Empty`.
* `sT(p)` which looks for a _triplet_ (3 adjacent cells),
  one of which is empty and the other two with the same bit `b`,
  in which case the empty cell must contain `1 - b`.

  The name `sT` stands for _strategy Triplet_.
* `sL(p)` which looks for a _line_ (a complete row or column of cells),
  half of them filled with bit `b` and one of them empty,
  in which case all empty cells must contain `1 - b`.

  The name `sL` stands for _strategy Line_.

A _metastrategy_ is a function that takes one or more strategies as parameters
and returns a strategy.
Some basic metastrategies are:
* `mP(s, t)` which returns a strategy that,
  when applied to puzzle `p`,
  first applies `s` to `p`, and then applies `t` to the result.

  The name `mP` stands for _metastrategy Pair_.
* `mPf(s, t)` which returns a strategy that,
  when applied to puzzle `p`,
  first applies `s` to `p`, and if there was change returns this as result,
  but if there was no change, then if applies `t` to `p`,
  and returns the result.
  
  The name `mPf` stands for _metastrategy Pair until first change_.
* `mF(s)` which keeps on applying s until there was no change.

  The name `mF` stands for _metastrategy Fixpoint_,
  since, by definition, `s` applied to `mF(s)` is the same as `mF(s)`.

The extra boolean result mentioned above
is especially useful for `mPf` and `mF`.
It is also useful to generalize `mP` and `mPf` to lists of strategies,
using a fold.

An idea for a more advanced metastrategy is
* `mG(s)` which looks for an empty cell,
  and tries successively a 0 and a 1 there,
  in each case applying strategy `s` afterwards
  (to do 'forced' updates)
  and if there was a rule violation (contradiction),
  updates the puzzle with the opposite bit.
  If no contradiction arises,
  then the puzzle is returned unchanged.

  The name `mG` stands for _metastrategy General contradiction_.

It turns out that `mG(sE)` behaves like a combination of
`sT` and `sL`.
The more powerful the strategy `s` in `mG(s)` is,
the better `mG(s)` works.

Let `mFG(s)` be `mF(mG(s))`, repeating `mG(s)` until no change.
Then applying `mFG` with itself as parameter
seems to be the most powerful strategy imaginable.
In Haskell, you cannot type such a function with self-application
(it gives rise to an infinite type).
But there is a way to accomplish this indirectly
through `fix mFG`.

There is, however, a snag.
Isn't there always when things look to good to be true?
`fix mFG` does work and it solves any puzzle with a unique solution.
But it isn't efficient:
* When applying `s` leads to an intermediate result with a contradiction,
  this is ignored until `s` completes.

  It would be better if strategies could return as extra data whether
  a contradiction occurred (like an exception).
  This then can shortcut all subsequent strategy applications
  (because they can't undo that contradiction).
* When applying `s` leads to an intermediate result that actually solves the puzzle,
  this is ignored (since it is not a contradiction).
  `mFG(mFG)` can happily conclude that a 0 does not lead to a contradiction
  (but an unnoticed solution),
  then try a 1 (which leads to a contradiction if the puzzle has a unique solution),
  and only then decide to update the puzzle with that 0 after all.

  It would be better if a strategies could return as extra result
  whether a solution was found (like an exception).
  This then can shortcut all subsequent strategy applications).

Handling this extra data appropriately under composition is exactly what
monads are for.


