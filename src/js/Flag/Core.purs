module Flag.Core (Event, Country, view, foldp, init) where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, replicate, zip, sortBy, drop, head)
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
type Countries = Array Country
type State = { countries ∷ Countries, correct ∷ Int, incorrect ∷ Int }

data Event = Answer Name | RequestCountries | ReceiveCountries (Either Error Countries)
newtype Country = Country { name :: Name, flag :: String }

init ∷ State
init = { countries: [], correct: 0, incorrect: 0 }

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM)
foldp (Answer name) st = noEffects $ case head st.countries of
  Nothing              -> st { countries = [], correct = 0, incorrect = 0 }
  Just (Country model) -> if name == model.name then isCorrect else isIncorrect
    where
      countries   = drop 1 st.countries
      isCorrect   = st { countries = countries, correct = st.correct + 1 }
      isIncorrect = st { countries = countries, incorrect = st.incorrect + 1 }
foldp (ReceiveCountries (Left err)) st = noEffects $ st { countries = [] }
foldp (ReceiveCountries (Right xs)) st = noEffects $ st { countries = xs }
foldp (RequestCountries)            st = { state: st, effects: [do
  xs <- liftEff' $ map fst <$> sortBy compareSnd <$> pairs
  pure $ Just $ ReceiveCountries xs
]}
  where
    pairs = zip countries <$> (sequence $ replicate (length countries) random)
    compareSnd (Tuple _ a) (Tuple _ b) = compare a b
    countries = [
      Country { name: "Russia", flag: "russia.svg" },
      Country { name: "Japan", flag: "japan.svg" },
      Country { name: "Thailand", flag: "thailand.svg" }
    ]

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
  case head state.countries of
    Nothing              -> div $ text "All done!"
    Just (Country model) -> do
      img ! src ("images/flags/" <> model.flag) ! alt "Flag"
      ul ! className "countries" $ for_ state.countries $ country

country ∷ Country → HTML Event
country (Country model) =
  li $ button #! onClick (const $ Answer model.name) $ text model.name
