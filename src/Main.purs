module Main where

import CSS.Color (red, green, yellow, blue)
import Control.Monad.Aff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Helpers (generateSequence, convertToColors)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Styles (buttonStyled)
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, start, onlyEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!), (!))


data Event = Start | Click | NewSequence (List String) | UserClick String

type AppEffects = (random :: RANDOM, console :: CONSOLE)

type State = 
  { sequence :: List String }

init :: State
init = { sequence: Nil }

foldp :: Event -> State -> EffModel State Event AppEffects
foldp (NewSequence list) state = 
  { state: state { sequence = list }
  , effects: [ logShow list *> pure Nothing ]
}

foldp Start state = { state: state, effects: [ do
  result <- liftEff generateSequence
  let colors = convertToColors result
  pure $ Just $ NewSequence colors
]}

foldp Click state = onlyEffects state [ log "click" *> pure Nothing ]
foldp (UserClick color) state = onlyEffects state [ log color *> pure Nothing ]

view :: State -> HTML Event
view count =
  div do
    div do
      div 
        ! buttonStyled red
        #! onClick (const $ UserClick "red")
        $ text "red"
      div 
        ! buttonStyled green
        #! onClick (const $ UserClick "green")
        $ text "green"
      div 
        ! buttonStyled yellow
        #! onClick (const $ UserClick "yellow")
        $ text "yellow"
      div 
        ! buttonStyled blue
        #! onClick (const $ UserClick "blue")
        $ text "blue"
    button #! onClick (const Start) $ text "Start"
    button #! onClick (const Click) $ text "Click"

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
