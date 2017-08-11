module App.Events
  ( Event(..)
  , Color
  , Sound
  ) where

import Data.List (List)
import Pux.DOM.Events (DOMEvent)

type Color = String
type Sound = String

data Event
  = Start
  | AnimateColor Color Sound
  | NewSequence (List String)
  | PlaySequence
  | UserClick Color
  | ResetColor
  | Reset
  | Strict DOMEvent
