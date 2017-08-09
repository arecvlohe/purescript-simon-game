module Main where

import Control.Monad.Aff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Helpers (generateSequence)
import Data.List.Lazy (List, nil)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, start, onlyEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!))


data Event = Start | Click | NewSequence (List Int)

type AppEffects = (random :: RANDOM, console :: CONSOLE)

type State = List Int

foldp :: Event -> State -> EffModel State Event AppEffects
foldp (NewSequence list) state = { state: list, effects: [ logShow list *> pure Nothing ]}
foldp Start state = { state: state, effects: [ do
  result <- liftEff generateSequence
  pure $ Just $ NewSequence result
]}
foldp Click state = onlyEffects state [ log "click" *> pure Nothing ]

view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Start) $ text "Start"
    button #! onClick (const Click) $ text "Click"

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: nil
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
