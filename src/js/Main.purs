module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Random (RANDOM)
import Flag.App (init, foldp, view)

main :: Eff (CoreEffects (ajax :: AJAX, random :: RANDOM)) Unit
main = do
  app <- start { initialState: init, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
