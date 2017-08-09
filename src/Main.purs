module Main where

import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List.Lazy (List, nil, replicateM)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!))


data Event = Start | NewSequence (List Int)

type AppEffects = (random :: RANDOM, console :: CONSOLE)

type State = List Int

foldp :: Event -> State -> EffModel State Event AppEffects
foldp Start state = { state: state, effects: [ do
  result <- liftEff (replicateM 20 (randomInt 1 4))
  pure $ Just $ NewSequence result
]}
foldp (NewSequence list) state = { state: list, effects: [ log "NewSequence" *> pure Nothing ]}


view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Start) $ text "Start"

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: nil
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
