module TextParser where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( on, targetValue, onClick)
import Http
import Json.Decode as Json
import Task
import Effects exposing (Effects, Never)
import StartApp as StartApp
import Regex exposing (split)
import Array

import StopWords exposing (words)


type Action = UpdateText String
  | GenerateSentences
  | PlaySentence Int
  | RequestGif
  | ReceiveGif (Maybe String)
  | NoOp
  --| StartSpeakSentence
  --| EndSpeakSentence

type alias Model =
    {
        inputText : String,
        sentences : List String,
        nextIndex: Int,
        currentGif : String
    }

initialModel : Model
initialModel =
    {
        inputText = "hello.world.this is very cool.yes.yes it is.",
        sentences = [],
        nextIndex = 0,
        currentGif = ""
    }

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
    case action of
        UpdateText newText ->
            ({ model | inputText = newText }, Effects.none)
        GenerateSentences ->
            ({ model | sentences = splitSentences model.inputText, nextIndex = 0 }, Effects.none)
        PlaySentence index ->
            (model, getRandomGif (Maybe.withDefault "SILENCE" (Array.get index (Array.fromList model.sentences))))
        RequestGif ->
            (model, Effects.none)
        ReceiveGif url ->
            ({ model | currentGif = (Maybe.withDefault "" url),
                       nextIndex = model.nextIndex + 1
             }, speak "FOOBAR IS A NICE PLACEHOLDER")
        NoOp ->
            (model, Effects.none)

splitSentences : String -> List String
splitSentences inputText =
    Regex.split Regex.All (Regex.regex "[.?!]") inputText

-- HTML structure
header : Html
header =
    div [class "header"]
        [ h1 [class "page-title"] [text "gif theatre"]
        ]

story : String -> Html
story text =
    p [class "story"] [Html.text text]

submitButton : Signal.Address Action -> Html
submitButton address =
    input [ class "submit"
        , type' "button"
        , value "Submit Story"
        , onClick address GenerateSentences ] []

playButton : Signal.Address Action -> Int -> Html
playButton address index =
    input [ class "play"
        , type' "button"
        , value "Play next"
        , onClick address (PlaySentence index) ] []

view address model =
    body []
      [ header
      , div [imgStyle model.currentGif] []
      , div [] (List.map story model.sentences)
      , input [placeholder "placeholder"
          , type' "text"
          , value model.inputText
          , on "input" targetValue (\val -> (Signal.message address (UpdateText val))) ] []
      , submitButton address
      , playButton address model.nextIndex
    ]

imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "margin" => "0 0 0 10vw"
    , "width" => "600px"
    , "height" => "400px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]


init : (Model, Effects.Effects Action)
init = (initialModel, Effects.none)


-- EFFECTS

(=>) = (,)


speak : String -> Effects Action
speak sentence =
  Signal.send spokenMailbox.address sentence
    |> Effects.task
    |> Effects.map (\_ -> NoOp)


spokenMailbox: Signal.Mailbox String
spokenMailbox =
    Signal.mailbox ""

getRandomGif : String -> Effects Action
getRandomGif topic =
  Http.get decodeUrl (randomUrl topic)
    |> Task.toMaybe
    |> Task.map ReceiveGif
    |> Effects.task


randomUrl : String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]


decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data", "image_url"] Json.string
