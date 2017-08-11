module Main where

import Prelude

import App.Update (foldp, init, AppEffects)
import App.View (view)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

main :: Eff (CoreEffects AppEffects) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
