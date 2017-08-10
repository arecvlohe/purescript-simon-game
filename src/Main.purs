module Main where

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Helpers (generateSequence, convertToColors)
import Data.List (List(..), snoc)
import Data.Maybe (Maybe(..))
import Data.Styles (buttonStyled)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (fromMaybe)
import DOM (DOM)
import DOM.HTML.HTMLMediaElement (play)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId)
import Prelude hiding (div, id)
import Pux (CoreEffects, EffModel, noEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, input, label, audio, source)
import Text.Smolder.HTML.Attributes (type', checked, src, id)
import Text.Smolder.Markup (text, (#!), (!), (!?))


data Event 
  = Start 
  | NewSequence (List String) 
  | UserClick String 
  | ResetColor 
  | Strict DOMEvent

type AppEffects = (random :: RANDOM, console :: CONSOLE, dom :: DOM)

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
  , effects: [ logShow (snoc state.userInput color) *> pure Nothing, 
    do
      delay $ Milliseconds 300.0
      pure $ Just $ ResetColor,
    do
      element <- getElementById $ ElementId color <> "Audio"
      play $ fromMaybe "" element
      pure Nothing
    ]
  }
foldp ResetColor state = noEffects $ state { currentColor = "" } 
foldp (Strict e) state = noEffects $ state { strict = not state.strict }


view :: State -> HTML Event
view state =
  div do
    div do
      div 
        ! buttonStyled "red" state.currentColor 
        #! onClick (const $ UserClick "red") $ do
        text "red"
        audio ! id "redAudio" $ do
          source 
            ! src "https://s3.amazonaws.com/freecodecamp/simonSound1.mp3"
            ! type' "audio/wav"
      div 
        ! buttonStyled "green" state.currentColor
        #! onClick (const $ UserClick "green") $ do
        text "green"
        audio ! id "greenAudio" $ do
          source 
            ! src "https://s3.amazonaws.com/freecodecamp/simonSound2.mp3"
            ! type' "audio/wav"
      div 
        ! buttonStyled "yellowSound" state.currentColor
        #! onClick (const $ UserClick "yellow") $ do
        text "yellow"
        audio ! id "yellowAudio" $ do
          source 
            ! src "https://s3.amazonaws.com/freecodecamp/simonSound3.mp3"
            ! type' "audio/wav"
      div 
        ! buttonStyled "blue" state.currentColor
        #! onClick (const $ UserClick "blue") $ do
        text "blue"
        audio ! id "blueAudio" $ do
          source 
            ! src "https://s3.amazonaws.com/freecodecamp/simonSound4.mp3"
            ! type' "audio/wav"
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
