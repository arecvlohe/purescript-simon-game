module Main where

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Data.Array (fromFoldable)
import Data.Helpers (generateSequence, convertToColors)
import Data.List (List(..), snoc, index, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sounds (play, SOUND)
import Data.Styles (buttonStyled)
import Data.Time.Duration (Milliseconds(..))
import Prelude hiding (div,id)
import Pux (CoreEffects, EffModel, noEffects, onlyEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, input, label)
import Text.Smolder.HTML.Attributes (type', checked)
import Text.Smolder.Markup (text, (#!), (!), (!?))


data Event 
  = Start
  | AnimateColor Number String
  | NewSequence (List String)
  | PlaySequence
  | UserClick String
  | ResetColor
  | Strict DOMEvent

type AppEffects = 
  ( random :: RANDOM
  , console :: CONSOLE
  , dom :: DOM
  , sound :: SOUND
  , timer :: TIMER
  )

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

generatePlaySequence :: forall e. Applicative e => Int -> List String -> Array (e (Maybe Event))
generatePlaySequence count sequence =
  range 0 (count - 1) #
  map (\v -> pure $ Just $ AnimateColor 300.0 (fromMaybe "" $ index sequence v) ) #
  fromFoldable

foldp :: Event -> State -> EffModel State Event AppEffects
foldp Start state = 
  if state.count > 0 then
    noEffects $ state
  else 
  { state: state { count = state.count + 1 }
  , effects: 
      [ do
          result <- liftEff generateSequence
          let colors = convertToColors result
          pure $ Just $ NewSequence colors
      ]
  }

foldp (NewSequence list) state =
  { state: state { sequence = list }
  , effects: [pure $ Just $ PlaySequence]
  }

foldp (UserClick color) state =
  { state: state { userInput = snoc state.userInput color, currentColor = color }
  , effects: [ pure $ Just $ AnimateColor 300.0 color ]
  }
foldp ResetColor state = noEffects $ state { currentColor = "" }

foldp (AnimateColor time color) state =
  { state: state { currentColor = color },
    effects: 
      [ liftEff $ play color *> pure Nothing
      , do 
        delay $ Milliseconds time
        pure $ Just $ ResetColor
      ]
  }

foldp PlaySequence state =
  onlyEffects state $ generatePlaySequence state.count state.sequence

foldp (Strict e) state = noEffects $ state { strict = not state.strict }


view :: State -> HTML Event
view state =
  div do
    div do
      div
        ! buttonStyled "red" state.currentColor 
        #! onClick (const $ UserClick "red")
        $ text ""
      div 
        ! buttonStyled "green" state.currentColor
        #! onClick (const $ UserClick "green")
        $ text ""
      div 
        ! buttonStyled "yellow" state.currentColor
        #! onClick (const $ UserClick "yellow")
        $ text ""
      div 
        ! buttonStyled "blue" state.currentColor
        #! onClick (const $ UserClick "blue")
        $ text ""
    div do
      text $ "Count" <> show state.count
    div do
      label $ text "Strict"
      (input !? state.strict) (checked "checked") ! type' "checkbox" #! onChange Strict 
    button #! onClick (const Start) $ text "Start"

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }
  
  renderToDOM "#app" app.markup app.input
