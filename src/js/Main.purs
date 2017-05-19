module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Flag.Core (view, foldp, initialState)

main ∷ ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start { initialState: initialState, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
