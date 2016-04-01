module Storyboard (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import TextParser

-- Model

type alias Model =
  { inputText : String
  , sentences : List String
  , playRequested : Bool
  }

initialModel : Model
initialModel =
  { inputText = ""
  , sentences = []
  , playRequested = False
  }

-- Actions

type Action
  = UpdateText String
  | GenerateSentences
  | PlayMovie

-- Updater

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    UpdateText newText ->
        ({ model | inputText = newText }, Effects.none)
    GenerateSentences ->
        ({ model | sentences = TextParser.splitSentences model.inputText }
        , Effects.none)
    PlayMovie ->
        ( {model | playRequested = True }, Effects.none )

-- View

-- HTML structure
header : Html
header =
  div [class "header"]
    [ h1 [class "page-title"] [text "gif theatre"]
    ]

submitButton : Signal.Address Action -> Html
submitButton address =
  input [ class "submit"
    , type' "button"
    , value "Submit Story"
    , onClick address GenerateSentences ] []

playButton : Signal.Address Action -> Html
playButton address =
  input [ class "play"
    , type' "button"
    , value "Play movie"
    , onClick address (PlayMovie) ] []

story : String -> Html
story text =
    p [class "story"] [Html.text text]

view address model =
  body []
    [ header
    , input [placeholder "placeholder"
      , type' "text"
      , value model.inputText
      , on "input" targetValue (\val -> (Signal.message address (UpdateText val))) ] []
    , submitButton address
    , playButton address
    , div [] (List.map story model.sentences)
  ]