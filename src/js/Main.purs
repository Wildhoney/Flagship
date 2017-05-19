module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Flag.Core (view, foldp, init)

main ∷ ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start { initialState: init, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
