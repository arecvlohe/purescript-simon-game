module Data.Helpers (
  generateSequence,
  convertToColors,
  checkUserInput
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List (List, slice, length)
import Data.Unfoldable (replicateA)


generateSequence :: ∀ fx. Eff (random :: RANDOM | fx) (List Int)
generateSequence =
  replicateA 20 (randomInt 1 4)

convertToColors :: List Int -> List String
convertToColors list =
  map (\a ->
    case a of
      1 -> "red"
      2 -> "green"
      3 -> "green"
      4 -> "blue"
      _ -> ""
  ) list

checkUserInput :: List String -> List String -> Boolean
checkUserInput sequence userInput =
  let
    seq = slice 0 (length userInput) sequence
  in
    seq == userInput
