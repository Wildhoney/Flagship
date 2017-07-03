module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Network.HTTP.Affjax (AJAX)
import Flag.App (init, foldp, view)

main ∷ Eff (CoreEffects (ajax ∷ AJAX)) Unit
main = do
  app <- start { initialState: init, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
