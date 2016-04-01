module MoviePlayer (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

-- Model

type alias Model =
  { sentences : List String
  }

initialModel : Model
initialModel =
  { sentences = []
  }

createModel : List String -> Model
createModel newSentences =
  { initialModel | sentences = newSentences
  }

-- Action

type Action
  = PlaySentence Int

-- Updater

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    PlaySentence _ ->
      ( model, Effects.none )

-- View

story : String -> Html
story text =
    p [class "story"] [Html.text text]

view address model =
  div [class "movie-player"] (List.map story model.sentences)

createPlayInput : Signal.Signal Int -> Signal.Signal Action
createPlayInput inputSignal =
  Signal.map PlaySentence inputSignal
