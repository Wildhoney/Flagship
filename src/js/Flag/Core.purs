module Flag.Core (Event, view, foldp, init) where

import Prelude
import Pux (EffModel)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt)

type Name = String
type State = { name ∷ Country }
data Event = Country Name

init ∷ State
init = { name: "Japan" }

foldp ∷ ∀ fx. Event → State → EffModel State Event fx
foldp (Country name) n = { state: { name: name }, effects: [] }

view ∷ State → HTML Event
view state =
  do
    h1 $ text "Which country has this flag?"
    img ! src flag ! alt "Flag"
    button #! onClick (const $ Country "Russia") $ text state.name
      where
        flag ∷ String
        flag = ("images/flags/" <> state.name <> ".svg")
