module Data.Helpers where

import Prelude

-- LIBRARIES

import Data.Unfoldable (replicateA)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)

generateSequence = 
  replicateA 20 (randomInt 1 4)
