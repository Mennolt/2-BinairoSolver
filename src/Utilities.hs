{-|
   Module       : Utilities
   Description  : Defines utility functions
   Copyright    : (c) Tom Verhoeff, 2021
   License      : None

   The "Utilities" module defines and exports these things:

   * Some functions on strings.
-}
module Utilities (
    splitStr
  , joinStr
  ) where

  {-|
    Function `splitStr` splits a string into a list of singleton strings.

    prop> splitStr "abc" = ["a", "b", "c"]
  -}
  splitStr :: String -> [String]
  splitStr = map (:[])

  {-|
    Function `joinStr` joins the strings in a list, inserting a given separator and
    putting a given terminator at the end.

    prop> joinStr sep term (splitStr "abc") = "a" ++ sep ++ "b" ++ sep ++ "c" ++ term
  -}
  joinStr :: String -> String -> [String] -> String
  joinStr sep term = foldr g term where
    s `g` t = error "TODO 1.1: complete the definition, use an `if`"
