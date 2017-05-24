module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Control.Monad.Eff.Random (RANDOM)
import Network.HTTP.Affjax (AJAX)
import Flag.Core (view, foldp, init)

main ∷ Eff (CoreEffects (random ∷ RANDOM, ajax ∷ AJAX)) Unit
main = do
  app <- start { initialState: init, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
