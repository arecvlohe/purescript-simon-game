module Data.Helpers (
  generateSequence,
  convertToColors
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List (List)
import Data.Unfoldable (replicateA)
import Data.Functor (map)

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
