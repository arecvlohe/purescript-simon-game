module Data.Helpers where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List.Lazy (List, replicateM)

generateSequence :: ∀ fx. Eff (random :: RANDOM) (List Int)
generateSequence =
  replicateM 20 (randomInt 1 4) 
