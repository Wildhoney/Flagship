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

type State = { all :: Countries, next :: Countries, current :: Countries, correct :: Int, incorrect :: Int }
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
init = { all: [], next: [], current: [], correct: 0, incorrect: 0 }

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
pick all current = pure <=< shuffle <<< (_ <> [next]) <<< take (answerCount - 1) <<<
                   filter (_ /= next) <=< shuffle $ all
  where next = maybe (Country { name: "_", flag: "_" }) id $ head current

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, random :: RANDOM)
foldp (ReceiveAnswer name) state = {
  state: case (_ == name) <<< maybe mempty (_.name <<< unwrap) $ head state.next of
           true -> state { next = drop 1 state.next, correct = state.correct + 1 }
           _    -> state { next = drop 1 state.next, incorrect = state.incorrect + 1 },
  effects: [pure <<< Just <<< ReceiveAnswers <=< liftEff $ pick state.all (drop 1 state.next)]
}
foldp (ReceiveAnswers current) state = noEffects state { current = current }
foldp (ReceiveCountries (Tuple all current)) state = noEffects state { all = all, next = all, current = current }
foldp (RequestCountries) state = {
  state: state { correct = 0, incorrect = 0 },
  effects: [Just <<< ReceiveCountries <$> fetch]
}

view :: State -> HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.next) <> "/" <> show (length state.all)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.all) of
      0 -> "Start"
      _ -> "Restart"
  section $ case (uncons state.next), (state.correct + state.incorrect) of
    Just { head }, _ -> do
      img ! (src <<< ("/images/flags/" <> _) <<< _.flag <<< unwrap $ head)
      ul $ for_ (_.name <<< unwrap <$> state.current) $ \name ->
        li ! key name #! onClick (const $ ReceiveAnswer name) $ text name
    Nothing, 0 -> div $ text mempty
    Nothing, _ -> do
      section ! className "results" $ do
        div ! className "percentage" $ text $ (show percentage) <> "%"
        div ! className "score" $ text $ "You scored " <> (show state.correct) <> " out of " <> (show total) <> "."
        a #! onClick (const RequestCountries) $ text "Play Again"
      where total = state.correct + state.incorrect
            percentage = round $ toNumber state.correct / (toNumber $ state.correct + state.incorrect) * 100.0
