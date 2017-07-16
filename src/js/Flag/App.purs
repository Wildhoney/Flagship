module Flag.App (Event, Country(..), shuffle, init, foldp, view) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Argonaut (class DecodeJson, decodeJson, Json, (.?))
import Data.Array (drop, filter, head, length, replicate, sortBy, take, uncons, zip)
import Data.Either (Either, either)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_, sequence)
import Data.Tuple (Tuple(..), snd)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

type State = { all :: Countries, remaining :: Countries, answers :: Countries, correct :: Int, incorrect :: Int }
type Countries = Array Country

data Event = RequestCountries | ReceiveCountries (Tuple Countries Countries) | ReceiveAnswer String | ReceiveAnswers Countries

newtype Country = Country { name :: String, flag :: String }

derive instance newtypeCountry :: Newtype Country _
derive instance eqCountry :: Eq Country

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
init = { all: [], remaining: [], answers: [], correct: 0, incorrect: 0 }

decode :: forall r. { response :: Json | r } -> Either String (Array Country)
decode request = decodeJson request.response :: Either String Countries

fetch :: forall e. Aff (ajax :: AJAX, random :: RANDOM | e) (Tuple Countries Countries)
fetch = get "/countries.json"
  >>= pure <<< decode
  >>= liftEff <<< shuffle <<< either (const []) id
  >>= \countries -> pure <<< Tuple countries <=< liftEff $ pick countries countries

shuffle :: forall e a. Array a -> Eff (random :: RANDOM | e) (Array a)
shuffle xs = do
  randoms <- sequence $ replicate (length xs) random
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

pick :: forall e. Countries -> Countries -> Eff (random :: RANDOM | e) Countries
pick all answers = shuffle <<< (_ <> [answer]) <<< take (answerCount - 1) <<< filter (_ /= answer) <=< shuffle $ all
  where answer = maybe (Country { name: "_", flag: "_" }) id $ head answers

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, random :: RANDOM)
foldp (ReceiveAnswer name) state = {
  state: case (_ == name) <<< maybe mempty (_.name <<< unwrap) $ head state.remaining of
           true -> state { remaining = drop 1 state.remaining, correct = state.correct + 1 }
           _    -> state { remaining = drop 1 state.remaining, incorrect = state.incorrect + 1 },
  effects: [pure <<< Just <<< ReceiveAnswers <=< liftEff $ pick state.all (drop 1 state.remaining)]
}
foldp (ReceiveAnswers answers) state = noEffects state { answers = answers }
foldp (ReceiveCountries (Tuple all answers)) state = noEffects state { all = all, remaining = all, answers = answers }
foldp (RequestCountries) state = {
  state: state { correct = 0, incorrect = 0 },
  effects: [Just <<< ReceiveCountries <$> fetch]
}

view :: State -> HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.remaining) <> "/" <> show (length state.all)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.all) of
      0 -> "Start"
      _ -> "Restart"
  section $ case (uncons state.remaining), (state.correct + state.incorrect) of
    Just { head }, _ -> do
      img ! (src <<< ("/images/flags/" <> _) <<< _.flag <<< unwrap $ head)
      ul $ for_ (_.name <<< unwrap <$> state.answers) $ \name ->
        li ! key name #! onClick (const $ ReceiveAnswer name) $ text name
    Nothing, 0 -> div $ text mempty
    Nothing, _ -> do
      section ! className "results" $ do
        div ! className "percentage" $ text $ (show percentage) <> "%"
        div ! className "score" $ text $ "You scored " <> (show state.correct) <> " out of " <> (show total) <> "."
        a #! onClick (const RequestCountries) $ text "Play Again"
      where total = state.correct + state.incorrect
            percentage = round $ toNumber state.correct / (toNumber $ state.correct + state.incorrect) * 100.0
