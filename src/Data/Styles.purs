module Data.Styles where

import Prelude

import CSS (Color, backgroundColor, height)
import CSS.Size (px)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.Markup (Attribute)

buttonStyled :: Color -> Attribute
buttonStyled color =
  style do 
    backgroundColor color
    height $ px 100.0
