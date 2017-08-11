module App.Styles
  ( buttonStyled
  ) where

import Prelude

import CSS (Color, backgroundColor, height, width)
import CSS.Color (red, green, yellow, blue, black, saturate, darken)
import CSS.Size (px)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.Markup (Attribute)

buttonStyled :: String -> String -> Attribute
buttonStyled color currentColor =
  let
    converted = convertColor color
    c = if currentColor == color && (color == "green" || color == "yellow" || color == "red")
        then saturate (-0.5) $ converted
      else if currentColor == color && color == "blue"
        then darken 0.1 $ converted
      else converted
  in
    style do
      backgroundColor c
      height $ px 200.0
      width $ px 200.0


convertColor :: String -> Color
convertColor color =
  case color of
    "red" -> red
    "green" -> green
    "yellow" -> yellow
    "blue" -> blue
    _ -> black

