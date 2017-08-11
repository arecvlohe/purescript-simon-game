module Main where

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (alert)
import DOM.HTML.Types (ALERT)
import Data.Array (fromFoldable, concat)
import Data.Helpers (generateSequence, convertToColors)
import Data.Int (toNumber)
import Data.List (List(..), snoc, index, range, length, slice)
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

type AppEffects =
  ( random :: RANDOM
  , console :: CONSOLE
  , dom :: DOM
  , sound :: SOUND
  , alert :: ALERT
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

checkUserInput :: List String -> List String -> Boolean
checkUserInput sequence userInput =
  let
    seq = slice 0 (length userInput) sequence
  in
    seq == userInput

generatePlaySequence :: forall e. Int -> List String -> Array (Aff e (Maybe Event))
generatePlaySequence count sequence =
  range 0 (count - 1) #
  map (\v ->
    [ do
        delay $ Milliseconds ((toNumber v + 1.0) * 1000.0)
        let color = fromMaybe "" $ index sequence v
        pure $ Just $ AnimateColor color color
    ]
  ) #
  fromFoldable #
  concat

foldp :: Event -> State -> EffModel State Event AppEffects
foldp Start state =
  if state.count > 20 then
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
  , effects: [ pure $ Just $ PlaySequence, logShow list *> pure Nothing ]
  }

foldp (UserClick color) state =
  let
    nextUserInput = snoc state.userInput color
    checksPass = checkUserInput state.sequence nextUserInput
  in
    if state.count < 1 then
      noEffects state
    else if checksPass && length nextUserInput == 20 then
      { state: init
      , effects:
        [ do
            wind <- liftEff window
            liftEff $ alert "You just won the game, yay! Let's do it again" wind
            pure Nothing
        , pure $ Just $ Start
        ]
      }
    else if checksPass && length nextUserInput == state.count then
      { state: state { userInput = Nil, count = state.count + 1 }
      , effects:
        [ pure $ Just $ AnimateColor color color
        , pure $ Just $ PlaySequence
        ]
      }
    else if checksPass then
      { state: state { userInput = nextUserInput }
      , effects:
        [ pure $ Just $ AnimateColor color color
        ]
      }
    else if not checksPass && state.strict then
      { state: state { userInput = Nil, count = 1 }
      , effects:
        [ pure $ Just $ AnimateColor color "error"
        , pure $ Just $ PlaySequence
        ]
      }
    else
      { state: state { userInput = Nil }
      , effects:
        [ pure $ Just $ AnimateColor color "error"
        , pure $ Just $ PlaySequence
        ]
      }

foldp ResetColor state = noEffects $ state { currentColor = "" }

foldp (AnimateColor color sound) state =
  { state: state { currentColor = color },
    effects:
      [ liftEff $ play sound *> pure Nothing
      , do
        delay $ Milliseconds 300.0
        pure $ Just $ ResetColor
      ]
  }

foldp PlaySequence state =
  onlyEffects state $ generatePlaySequence state.count state.sequence

foldp (Strict e) state = noEffects $ state { strict = not state.strict }

foldp Reset state =
  { state: init
  , effects: [ pure $ Just $ Start ]
  }


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
    button #! onClick (const Reset) $ text "Reset"

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
