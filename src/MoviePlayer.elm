module MoviePlayer (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task exposing (Task, andThen, sleep, succeed)
import Array
import Debug exposing (log)
import TextParser exposing (constructSearchTermFromSentence)
-- Model

type alias Model =
  { sentences : List String
  , currentGif : String
  , currentSentence : String
  , isFinished: Bool
  }

initialGif : String
initialGif = "https://media.giphy.com/media/FUwnn0N8EzDvW/giphy.gif"

createModel : List String -> Model
createModel newSentences =
  { sentences = newSentences
  , currentGif = "https://media.giphy.com/media/FUwnn0N8EzDvW/giphy.gif"
  , currentSentence = "ssh... silence"
  , isFinished = False
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
          ({model | isFinished = True
                  , currentGif = ""
                  , currentSentence = ""
           }
           , Effects.none
          )
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
    [ div [imgStyle model.currentGif] [
      div [finishedStyle model.isFinished] [ text "Fin." ]
    ]
    , div [subtitleStyle] [ text model.currentSentence ]
    ]

finishedStyle : Bool -> Attribute
finishedStyle isFinished =
  style
    [ "display" => if isFinished then "block" else "none"
    , "color" => "white"
    , "font-family" => "Helvetica"
    , "font-size" => "56px"
    , "font-weight" => "bold"
    , "position" => "absolute"
    , "width" => "100%"
    , "top" => "38%"
    , "text-align" => "center"]

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
  Task.sleep 5000 -- wait a litte to show the coundown timer
    |> Effects.task
    |> Effects.map (always (PlaySentence 0))
