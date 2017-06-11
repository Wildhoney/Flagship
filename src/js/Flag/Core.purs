module Flag.Core (fetch, shuffle, view, foldp, init) where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Argonaut (decodeJson, Json)
import Data.Array (length, replicate, sortBy, zip, head, take, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Flag.Types (Countries, Country(..), Event(..), State)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (h1, button, ul, li, div, img)
import Text.Smolder.HTML.Attributes (className, src, alt)
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

fetch ∷ ∀ e. Aff (random ∷ RANDOM, ajax ∷ AJAX | e) (Maybe Countries)
fetch = get "/countries.json" >>= \xs → case decode xs of
          Left _          → pure $ Nothing
          Right countries → pure $ Just countries

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM, ajax ∷ AJAX)
foldp (Request)           state = onlyEffects state [Just <<< Receive <$> fetch]
foldp (Receive Nothing)   state = noEffects $ state { countries = Nothing }
foldp (Receive (Just xs)) state = noEffects $ state { countries = Just xs }
foldp (Respond name)      state = noEffects $ case join $ uncons <$> state.countries of
  Nothing                              → state { countries = Nothing }
  Just { head: (Country x), tail: xs } → case name == x.name of
    true  → state { countries = Just xs, correct = state.correct + 1 }
    false → state { countries = Just xs, incorrect = state.incorrect + 1 }

view ∷ State → HTML Event
view state = div ! className "flagship" $ do
  ul ! className "header" $ do
    li $ h1 $ text "Flagship"
    li $ button #! onClick (const Request) $ text "Start"
    li ! className "score correct" $ text $ show state.correct
    li ! className "score incorrect" $ text $ show state.incorrect
    li ! className "score" $ text $ show $ state.correct + state.incorrect
  <> do div ! className "prompt" $ case state.countries of
          Nothing        → button #! onClick (const Request) $ text "Start"
          Just countries → case head countries of
            Nothing              → div $ text "All done!"
            Just (Country current) → do
              img ! src ("images/flags/" <> current.flag) ! alt "Flag"
              ul ! className "countries" $ for_ (take 4 countries) $ \(Country answer) →
                li $ button #! onClick (const $ Respond answer.name) $ text answer.name
