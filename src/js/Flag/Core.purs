module Flag.Core (Event, view, foldp, init) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (length)
import Pux (EffModel)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt)

type Name = String
type Countries = Array Name

type State = { name ∷ Name, countries ∷ Countries }
data Event = Country Name | RequestCountries | ReceiveCountries Countries

init ∷ State
init = { name: "Japan", countries: [] }

foldp ∷ ∀ fx. Event → State → EffModel State Event fx
foldp (Country x)           s = { state: { name: x, countries: s.countries }, effects: [] }
foldp (ReceiveCountries xs) s = { state: { name: s.name, countries: xs }, effects: [] }
foldp (RequestCountries)    s = { state: { name: s.name, countries: [] }, effects: [do
  pure $ Just $ ReceiveCountries ["Russia", "Japan", "Thailand"]
]}

view ∷ State → HTML Event
view state =
  do
    h1 $ text ("Which country has this flag?" <> (show $ length state.countries))
    img ! src flag ! alt "Flag"
    button #! onClick (const $ Country "Russia") $ text state.name
    button #! onClick (const RequestCountries) $ text "Start"
      where
        flag = ("images/flags/" <> state.name <> ".svg")
