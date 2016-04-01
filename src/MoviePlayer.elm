module MoviePlayer (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task exposing (Task)
import Array
import Debug exposing (log)
import TextParser exposing (constructSearchTermFromSentence)
-- Model

type alias Model =
  { sentences : List String
  , currentGif : String
  , currentSentence : String
  }

createModel : List String -> Model
createModel newSentences =
  { sentences = newSentences
  , currentGif = ""
  , currentSentence = ""
  }

createPlayInput : Signal.Signal Int -> Signal.Signal Action
createPlayInput inputSignal =
  Signal.map PlaySentence inputSignal

initialModel : Model
initialModel =
  createModel []

-- Action

type Action
  = PlaySentence Int
  | RequestGif
  | ReceiveGif (Maybe String)
  | NoOp

-- Updater

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    PlaySentence _ ->
      case model.sentences of
        [] ->
          (model, Effects.none)
        [sentence] ->
          ({ model | currentSentence = sentence, sentences = [] }
          , getGif (constructSearchTermFromSentence sentence))
        sentence::rest ->
          ({ model | currentSentence = sentence , sentences = rest }
          , getGif (constructSearchTermFromSentence sentence))

    RequestGif ->
      (model, Effects.none)

    ReceiveGif url ->
      ({ model | currentGif = (Maybe.withDefault "" url) }
       , speak model.currentSentence
      )
    NoOp ->
      (model, Effects.none)
-- View

header : Html
header =
    div [class "header"]
        [ h1 [class "page-title"] [text "gif theatre"]
        ]

story : String -> Html
story text =
    p [class "story"] [Html.text text]

view address model =
  div [backgroundStyle]
    [ div [imgStyle model.currentGif] []
    , div [subtitleStyle] [ text model.currentSentence ]
    ]

subtitleStyle : Attribute
subtitleStyle =
  style
    [ "font-family" => "Helvetica"
    , "position" => "absolute"
    , "font-family" => "Helvetica"
    , "font-size" => "34px"
    , "color" => "white"
    , "font-weight" => "bold"
    , "margin" => "2vw"
    , "-webkit-text-stroke" => "1px black"
    , "bottom" => "33px"
    , "text-align" => "center"
    , "width" => "96vw"
    ]

backgroundStyle : Attribute
backgroundStyle =
  style
    [ "width" => "100vw"
    , "height" => "100vh"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => "url(http://i.imgur.com/zMmMATM.jpg?1)"
    ]

imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "409px"
    , "height" => "231px"
    , "position" => "absolute"
    , "top" => "25%"
    , "left" => "45.5%"
    , "border-radius" => "4px"
    , "background-color" => "black"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++  url ++ "')")
    , "transform" => "rotate(-1.2deg) skew(-2deg)"
    , "z-index" => "1000"
    ]
-- EFFECTS

(=>) = (,)

getGif : String -> Effects Action
getGif query =
  Http.get decodeUrl (randomUrl query)
    |> Task.toMaybe
    |> Task.map ReceiveGif
    |> Effects.task

randomUrl : String -> String
randomUrl query =
  Http.url "http://api.giphy.com/v1/gifs/translate"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "s" => query
    ]

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data", "images", "original", "url"] Json.string


speak : String -> Effects Action
speak sentence =
  Signal.send spokenMailbox.address sentence
    |> Effects.task
    |> Effects.map (\_ -> NoOp)

spokenMailbox: Signal.Mailbox String
spokenMailbox =
    Signal.mailbox ""

startMovie : Effects Action
startMovie =
  Task.succeed 0
    |> Effects.task
    |> Effects.map PlaySentence
