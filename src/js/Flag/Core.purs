module Flag.Core (Event, view, foldp, init) where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, replicate, zip, sortBy)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Aff (liftEff')
import Data.Tuple (Tuple(..), fst)
import Data.Traversable (sequence)
import Data.Foldable (for_)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button, ul, li, div)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt, className, disabled)

type Name = String
type Countries = Array Name

type State = { name ∷ Name, countries ∷ Countries, correct ∷ Int, incorrect ∷ Int }
data Event = Answer Name | RequestCountries | ReceiveCountries (Either Error Countries)

init ∷ State
init = { name: "", countries: [], correct: 0, incorrect: 0 }

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM)
foldp (Answer x) st
  | st.name == x = noEffects $ st { name = x, correct = st.correct + 1 }
  | st.name /= x = noEffects $ st { name = x, incorrect = st.incorrect + 1 }
  | otherwise    = noEffects $ st { name = x }
foldp (ReceiveCountries (Left err)) st = noEffects $ st { countries = [] }
foldp (ReceiveCountries (Right xs)) st = noEffects $ st { name = "Russia", countries = xs }
foldp (RequestCountries)            st = { state: st, effects: [do
  xs <- liftEff' $ map fst <$> sortBy compareSnd <$> zip countries <$> (sequence $ replicate (length countries) random)
  pure $ Just $ ReceiveCountries xs
]}
  where
    compareSnd (Tuple _ a) (Tuple _ b) = compare a b
    countries = ["Russia", "Japan", "Thailand"]


view ∷ State → HTML Event
view state = div ! className "flagship" $ case uncons state.countries of
  Nothing -> toolbar state
  Just _  -> toolbar state <> flag state

toolbar ∷ State → HTML Event
toolbar state = ul ! className "header" $ do
  li $ h1 $ text "Flagship"
  li $ button ! disabled isDisabled #! onClick (const RequestCountries) $ text "Start"
  li ! className "score correct" $ text $ show state.correct
  li ! className "score incorrect" $ text $ show state.incorrect
  li ! className "score" $ text $ show $ state.correct + state.incorrect
    where
      isDisabled = if (length state.countries == 0) then "" else "disabled"

flag ∷ State → HTML Event
flag state = div ! className "prompt" $ do
  img ! src path ! alt "Flag"
  ul ! className "countries" $ for_ state.countries choice
    where
      path = ("images/flags/" <> state.name <> ".svg")

choice ∷ Name → HTML Event
choice name =
  li $ button #! onClick (const $ Answer name) $ text name
