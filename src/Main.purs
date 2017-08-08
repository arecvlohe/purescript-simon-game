module Main where

import Prelude hiding (div)

-- LIBRARIES

import Control.Monad.Eff (Eff)
import Data.List (List(..), (:))
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!))

-- LOCAL IMPORTS

import Data.Helpers (generateSequence)

data Event = Start

type State = List Int

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Start n = { state: (1 : 2 : Nil), effects: [ ] }

view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Start) $ text "Start"

main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: (1 : Nil)
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
