module Flag.App (Event, Country(..), shuffle, init, foldp, view) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (class DecodeJson, decodeJson, Json, (.?))
import Data.Array (drop, head, length, replicate, sortBy, take, uncons, zip)
import Data.Either (Either, either)
import Data.Int (fromNumber, round)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_, sequence)
import Data.Tuple (Tuple(..), snd)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

type State = { all :: Countries, current :: Array String, correct :: Number, incorrect :: Number }
type Countries = Array Country

data Event = RequestCountries |
             ReceiveCountries (Tuple Countries (Array String)) |
             ReceiveAnswer String |
             ReceiveAnswers (Array String)

newtype Country = Country { name :: String, flag :: String }

derive instance newtypeCountry :: Newtype Country _

instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

instance decodeJsonCountry :: DecodeJson Country where
  decodeJson json = do
    model <- decodeJson json
    name <- model .? "name"
    flag <- model .? "flag"
    pure $ Country { name, flag }

answerCount :: Int
answerCount = 4

init :: State
init = { all: [], current: [], correct: 0.0, incorrect: 0.0 }

decode :: ∀ r. { response :: Json | r } -> Either String (Array Country)
decode request = decodeJson request.response :: Either String Countries

fetch :: forall e. Aff (ajax :: AJAX, random :: RANDOM | e) (Tuple Countries (Array String))
fetch = get "/countries.json"
  >>= pure <<< decode
  >>= liftEff <<< shuffle <<< either (const []) id
  >>= \countries -> pure <<< Tuple countries <=< liftEff <<< pick $ countries

shuffle :: forall e a. Array a -> Eff (random :: RANDOM | e) (Array a)
shuffle xs = do
  randoms <- sequence $ replicate (length xs) random
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

pick :: forall e. Countries -> Eff (random :: RANDOM | e) (Array String)
pick xs = pure <=< shuffle <<< (next <> _) <<< take (answerCount - 1) <=< shuffle $ _.name <<< unwrap <$> xs
  where next = _.name <<< unwrap <$> take 1 xs

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, random :: RANDOM)
foldp (ReceiveAnswer name) state = {
  state: case (_ == name) <<< maybe "" (_.name <<< unwrap) $ head state.all of
           true -> state { all = drop 1 state.all, correct = state.correct + 1.0 }
           _    -> state { all = drop 1 state.all, incorrect = state.incorrect + 1.0 },
  effects: [pure <<< Just <<< ReceiveAnswers <=< liftEff <<< pick <<< drop 1 $ state.all]
}
foldp (ReceiveAnswers current) state = noEffects state { current = current }
foldp (ReceiveCountries (Tuple all current)) state = noEffects state { all = all, current = current }
foldp (RequestCountries) state = {
  state: state { correct = 0.0, incorrect = 0.0 },
  effects: [Just <<< ReceiveCountries <$> fetch]
}

view :: State -> HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.all)
    div ! className "correct" $ text $ show correct
    div ! className "incorrect" $ text $ show incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.all) of
      0 -> "Start"
      _ -> "Restart"
  section $ case (uncons state.all), (state.correct + state.incorrect) of
    Just { head }, _ -> do
      img ! (src <<< ("/images/flags/" <> _) <<< _.flag <<< unwrap $ head)
      ul $ for_ state.current $ \name -> li #! onClick (const $ ReceiveAnswer name) $ text name
    Nothing, 0.0 -> div $ text ""
    Nothing, _   -> do
      section ! className "results" $ do
        div ! className "percentage" $ text $ (show percentage) <> "%"
        div ! className "score" $ text $ "You scored " <> (show correct) <> " out of " <> (show total) <> "."
        a #! onClick (const RequestCountries) $ text "Play Again"
      where total = maybe 0 id <<< fromNumber $ (state.correct + state.incorrect)
            correct = maybe 0 id <<< fromNumber $ state.correct
            incorrect = maybe 0 id <<< fromNumber $ state.incorrect
            percentage = round $ (state.correct / (state.correct + state.incorrect)) * 100.0
