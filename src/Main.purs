module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Helpers (generateSequence)
import Data.List.Lazy (List, nil)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, noEffects, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!))

data Event = Start | NewSequence (List Int)

type State = List Int

foldp :: ∀ fx. Event -> State -> EffModel State Event (random :: RANDOM)
foldp (NewSequence list) state =
  noEffects $ list
foldp Start state = { state: state, effects: [ do
  result <- liftEff generateSequence
  pure $ Just $ NewSequence result
]}


view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Start) $ text "Start"

main :: ∀ fx. Eff (CoreEffects fx (random :: RANDOM)) Unit
main = do
  app <- start
    { initialState: nil
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
