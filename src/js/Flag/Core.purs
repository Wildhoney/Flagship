module Flag.Core (fetch, shuffle, view, foldp, init) where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Argonaut (decodeJson, Json)
import Data.Array (length, replicate, sortBy, zip)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Flag.Types (Countries, Country(..), State, Event(..))
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (h1, button, ul, li, div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

init ∷ State
init = { countries: Nothing, correct: 0, incorrect: 0 }

shuffle ∷ ∀ e a. Array a → Eff (random :: RANDOM | e) (Array a)
shuffle xs = do
  randoms <- sequence $ replicate (length xs) random
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

decode ∷ ∀ r. { response ∷ Json | r } → Either String (Array Country)
decode request = decodeJson request.response :: Either String Countries

fetch ∷ ∀ e. Aff (ajax ∷ AJAX | e) (Maybe Event)
fetch = do
  response <- attempt $ get "/countries.json"
  let countries = either (Left <<< show) decode response
  pure $ Just $ Receive $ case shuffle <$> countries of
    Left _   → Nothing
    Right xs → Just xs

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM, ajax ∷ AJAX)
foldp (Respond name)      state = noEffects $ state { countries = Nothing, correct = state.correct + 1 }
foldp (Receive Nothing)   state = noEffects $ state { countries = Nothing }
foldp (Receive (Just xs)) state = noEffects $ state { countries = Just xs }
foldp (Request)           state = { state, effects: [fetch] }

view ∷ State → HTML Event
view state = div ! className "flagship" $ case state.countries of
  Nothing → toolbar state
  Just _  → toolbar state <> country state

toolbar ∷ State → HTML Event
toolbar state = ul ! className "header" $ do
  li $ h1 $ text "Flagship"
  li $ button #! onClick (const Request) $ text "Start"
  li ! className "score correct" $ text $ show state.correct
  li ! className "score incorrect" $ text $ show state.incorrect
  li ! className "score" $ text $ show $ state.correct + state.incorrect
    -- where isDisabled = state.countries >>= \xs → pure $ if (length xs == 0) then "" else "disabled"

country ∷ State → HTML Event
country state = div ! className "prompt" $ do
  case state.countries of
    Nothing        → button #! onClick (const Request) $ text "Start"
    Just _         → button $ text "Test"
    -- Just countries → liftEff $ countries >>= \countries -> case head countries of
    --   Nothing              → pure $ div $ text "All done!"
    --   Just (Country model) → pure $ do
    --     img ! src ("images/flags/" <> model.flag) ! alt "Flag"
    --     ul ! className "countries" $ for_ (take 4 countries) $ answer

answer ∷ Country → HTML Event
answer (Country model) =
  li $ button #! onClick (const $ Respond model.name) $ text model.name
