module App.Update
  ( foldp
  , init
  , AppEffects
  , State
  ) where

import Prelude

import App.Events (Event(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (fromFoldable, concat)
import Data.Helpers (generateSequence, convertToColors, checkUserInput)
import Data.Int (toNumber)
import Data.List (List(..), range, index, snoc, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sounds (SOUND, play)
import Data.Time.Duration (Milliseconds(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert)
import Pux (EffModel, noEffects, onlyEffects)

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
  , effects: [ pure $ Just $ PlaySequence ]
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
