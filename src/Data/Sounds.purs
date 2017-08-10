module Data.Sounds where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

-- INFLUENCED BY BODIL'S PURESCRIPT IS MAGIC
-- https://github.com/bodil/purescript-is-magic/blob/master/src/Sound.purs

foreign import data SOUND :: Effect

foreign import play :: forall e. String -> Eff (sound :: SOUND| e) Unit

