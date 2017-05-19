module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (h1, div, img)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.HTML.Attributes (src, alt)

type Country = String
type State = { name ∷ Country }
data Event = Present Country

-- | Return a new state (and effects) from each event
foldp ∷ ∀ fx. Event → State → EffModel State Event fx
foldp (Present name) n = { state: { name: name }, effects: [] }

-- | Return markup from the state
view ∷ State → HTML Event
view state =
  do
    h1 $ text "Flagship"
    img ! src ("images/flags/" <> state.name <> ".svg") ! alt "Flag"
    div $ text state.name

-- | Start and render the app
main ∷ ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start { initialState: initial, view, foldp, inputs: [] }
  renderToDOM ".app" app.markup app.input
    where initial = { name: "Japan" }
