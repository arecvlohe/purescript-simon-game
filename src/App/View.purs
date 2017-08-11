module App.View
  ( view
  ) where

import Prelude hiding (div,id)

import App.Events (Event(..))
import App.Update (State)
import App.Styles (buttonStyled)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, input, label, h1)
import Text.Smolder.HTML.Attributes (type', checked, className)
import Text.Smolder.Markup (text, (#!), (!), (!?))

view :: State -> HTML Event
view state =
  div ! className "container" $ do
    h1 $ text "Simon Game in PureScript"
    div ! className "group" $ do
      div
        ! buttonStyled "red" state.currentColor
        #! onClick (const $ UserClick "red")
        $ text ""
      div
        ! buttonStyled "green" state.currentColor
        #! onClick (const $ UserClick "green")
        $ text ""
    div ! className "group" $ do
      div
        ! buttonStyled "yellow" state.currentColor
        #! onClick (const $ UserClick "yellow")
        $ text ""
      div
        ! buttonStyled "blue" state.currentColor
        #! onClick (const $ UserClick "blue")
        $ text ""
    div ! className "count" $ do
      text $ "Count" <> " " <> show state.count
    div do
      label ! className "strict" $ text "Strict"
      (input !? state.strict) (checked "checked") ! type' "checkbox" #! onChange Strict
    div do
      button ! className "outline start" #! onClick (const Start) $ text "Start"
      button ! className "outline reset" #! onClick (const Reset) $ text "Reset"
