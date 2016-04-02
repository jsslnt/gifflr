module Storyboard (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import TextParser

import VoiceSettings

-- Model

type alias Model =
  { inputText : String
  , sentences : List String
  , playRequested : Bool
  , voiceSettingsModel : VoiceSettings.SettingsModel
  }

initialModel : Model
initialModel =
  { inputText = ""
  , sentences = []
  , playRequested = False
  , voiceSettingsModel = VoiceSettings.initialModel
  }

-- Actions

type Action
  = UpdateText String
  | GenerateSentences
  | PlayMovie
  | VoiceSettingsAction VoiceSettings.Action

-- Updater

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    UpdateText newText ->
        ({ model | inputText = newText }, Effects.none)
    GenerateSentences ->
        ({ model | sentences = TextParser.splitTextBlock model.inputText, playRequested = True }
        , Effects.none)
    PlayMovie ->
        ( {model | playRequested = True }, Effects.none )
    VoiceSettingsAction childAction ->
      let
        (result, fx) = VoiceSettings.update childAction model.voiceSettingsModel
      in
        ({ model | voiceSettingsModel = result }
        , Effects.map VoiceSettingsAction fx
        )

-- View

-- HTML structure

header: Html
header =
  h2 [headerStyle] [Html.text "Movie generator extraordinaire!"]

submitButton : Signal.Address Action -> Html
submitButton address =
  input [ buttonStyle
    , type' "button"
    , value "Submit Story"
    , onClick address GenerateSentences ] []

playButton : Signal.Address Action -> Html
playButton address =
  input [ buttonStyle
    , type' "button"
    , value "Play movie"
    , onClick address (PlayMovie) ] []

story : String -> Html
story text =
    --p [class "story"] [Html.text text]
    let
        words = TextParser.filterStopWords (TextParser.splitSentence text)
    in
        p   [class "story"]
            [ ul [] (List.map span words) ]

span : String -> Html
span word =
    li [] [Html.text word]

view address model =
  div [ boardStyle model.playRequested ]
    [ div [backDropStyle] []
    , div [containerStyle] [
      header
      , textarea [placeholder "Pitch your movie idea here!"
        , textAreaStyle
        , rows 3
        , type' "text"
        , value model.inputText
        , on "input" targetValue (\val -> (Signal.message address (UpdateText val))) ] []
      , VoiceSettings.view (Signal.forwardTo address VoiceSettingsAction) model.voiceSettingsModel
      , submitButton address
      ]

  ]

(=>) = (,)

textAreaStyle : Attribute
textAreaStyle =
  style
    [ "resize" => "none"
    , "width" => "60%"
    , "background-color" => "whitesmoke"
    , "border" => "none"
    , "border-bottom" => "2px solid grey"
    , "margin-top" => "40px"
    , "margin-bottom" => "40px"
    ]

boardStyle : Bool -> Attribute
boardStyle playRequested =
  style
    [ "display" => "inline-block"
    , "position" => "absolute"
    , "top" => "0"
    , "width" => "100vw"
    , "height" => "100vh"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('http://i.vimeocdn.com/video/156054460_1280x720.jpg')")
    , "transform" => if playRequested then "translateY(-120vh)" else ""
    , "transition" => "transform 1.6s cubic-bezier(0.64, 0.66, 0.85,-0.75)"
    , "z-index" => "1000"
    ]

containerStyle : Attribute
containerStyle =
  style
    [ "position" => "relative"
    , "text-align" => "center"
    , "background-color" => "whitesmoke"
    , "margin" => "10vh 0 0 10vw"
    , "border" => "1px solid gold"
    , "width" => "80vw"
    , "height" => "60vh"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "z-index" => "999"
    ]

headerStyle : Attribute
headerStyle =
  style
    [ "color" => "gold"
    , "position" => "absolute"
    , "top" => "-86px"
    , "left" => "50"
    , "font-size" => "3em"
    , "-webkit-text-stroke" => "1px black"
    ]

buttonStyle : Attribute
buttonStyle =
  style
    [ "position" => "absolute"
    , "bottom" => "20px"
    , "right" => "20px"
    ]

backDropStyle : Attribute
backDropStyle =
  style [ "position" => "absolute"
  , "width" => "100%"
  , "height" => "100%"
  , "background-color" => "rgba(0,0,0,0.6)"
  ]
