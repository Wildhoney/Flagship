module Flag.Core (Event, view, foldp, initialState) where

import Prelude hiding (div)
import Pux (EffModel)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (h1, div, img)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.HTML.Attributes (src, alt)

type Country = String
type State = { name ∷ Country }
data Event = Present Country

initialState ∷ State
initialState = { name: "Argentina" }

foldp ∷ ∀ fx. Event → State → EffModel State Event fx
foldp (Present name) n = { state: { name: name }, effects: [] }

view ∷ State → HTML Event
view state =
  do
    h1 $ text "Flagship"
    img ! src ("images/flags/" <> state.name <> ".svg") ! alt "Flag"
    div $ text state.name
