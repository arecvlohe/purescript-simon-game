module Main where

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Helpers (generateSequence, convertToColors)
import Data.List (List(..), snoc)
import Data.Maybe (Maybe(..))
import Data.Styles (buttonStyled)
import Data.Time.Duration (Milliseconds(..))
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, noEffects, onlyEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, input, label)
import Text.Smolder.HTML.Attributes (type', checked)
import Text.Smolder.Markup (text, (#!), (!), (!?))


data Event 
  = Start 
  | NewSequence (List String) 
  | UserClick String 
  | ResetColor 
  | Strict DOMEvent 
  | Click

type AppEffects = (random :: RANDOM, console :: CONSOLE)

type State = 
  { sequence :: List String
  , userInput :: List String
  , currentColor :: String
  , count :: Int
  , strict :: Boolean
  }

init :: State
init = 
  { sequence: Nil
  , userInput: Nil
  , currentColor: ""
  , count : 0
  , strict: false
  }

foldp :: Event -> State -> EffModel State Event AppEffects
foldp Start state = 
  { state: state
  , effects: [ do
      result <- liftEff generateSequence
      let colors = convertToColors result
      pure $ Just $ NewSequence colors
    ]
  }
foldp (NewSequence list) state = 
  { state: state { sequence = list }
  , effects: [ logShow list *> pure Nothing ]
  }
foldp (UserClick color) state = 
  { state: state { userInput = snoc state.userInput color, currentColor = color }
  , effects: [ logShow (snoc state.userInput color) *> pure Nothing, do
      delay $ Milliseconds 300.0
      pure $ Just $ ResetColor
    ]
  }
foldp ResetColor state = noEffects $ state { currentColor = "" } 
foldp (Strict e) state = noEffects $ state { strict = not state.strict }
foldp Click state = onlyEffects state [ log "click" *> pure Nothing ]


view :: State -> HTML Event
view state =
  div do
    div do
      div 
        ! buttonStyled "red" state.currentColor
        #! onClick (const $ UserClick "red")
        $ text "red"
      div 
        ! buttonStyled "green" state.currentColor
        #! onClick (const $ UserClick "green")
        $ text "green"
      div 
        ! buttonStyled "yellow" state.currentColor
        #! onClick (const $ UserClick "yellow")
        $ text "yellow"
      div 
        ! buttonStyled "blue" state.currentColor
        #! onClick (const $ UserClick "blue")
        $ text "blue"
    div do
      label $ text "Strict"
      (input !? state.strict) (checked "checked") ! type' "checkbox" #! onChange Strict 
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
