module App.View
  ( view
  ) where

import Prelude hiding (div,id)

import App.Events (Event(..))
import App.Update (State)
import Data.Styles (buttonStyled)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, input, label)
import Text.Smolder.HTML.Attributes (type', checked)
import Text.Smolder.Markup (text, (#!), (!), (!?))

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
      text $ "Count" <> " " <> show state.count
    div do
      label $ text "Strict"
      (input !? state.strict) (checked "checked") ! type' "checkbox" #! onChange Strict
    button #! onClick (const Start) $ text "Start"
    button #! onClick (const Reset) $ text "Reset"
