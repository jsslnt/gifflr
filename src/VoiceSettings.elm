module VoiceSettings (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

-- SettingsModel

type alias SettingsModel =
  { voice: String
  , rate: String
  , pitch: String
  }

initialModel : SettingsModel
initialModel =
  { voice = "Alex"
  , rate = "1.0"
  , pitch = "1.0"
  }

-- Action

type Action
  = SetVoice String
  | SetRate String
  | SetPitch String
  | NoOp

-- Updater

update : Action -> SettingsModel -> ( SettingsModel, Effects.Effects Action )
update action model =
  case action of

    SetVoice newVoice ->
      ({ model | voice = newVoice }, changeVoice model)

    SetRate newRate ->
      ({ model | rate = newRate }, changeVoice model )

    SetPitch newPitch ->
      ({ model | pitch = newPitch }, changeVoice model )

    NoOp ->
      (model, Effects.none)

-- View

voiceOption : String -> Html
voiceOption voice =
    option [value voice] [Html.text voice]

voiceSelect : Signal.Address Action -> List String -> Html
voiceSelect address voices =
    div [] [
        p [] [Html.text "As narrated by"]
      , select [on "input" targetValue (\val -> (Signal.message address (SetVoice val)))] (List.map voiceOption voices)
    ]

rateSelect : Signal.Address Action -> String -> Html
rateSelect address rate =
    div [] [
        p [] [Html.text "Rate of speech"]
      , input [
          type' "range"
        , rangeStyle
        , Html.Attributes.min "0.1"
        , Html.Attributes.max "11"
        , on "input" targetValue (\val -> (Signal.message address (SetRate val)))] []
    ]

pitchSelect : Signal.Address Action -> String -> Html
pitchSelect address voices =
    div [] [
        p [] [Html.text "Pitch"]
      , input [
          type' "range"
        , rangeStyle
        , Html.Attributes.min "0.1"
        , Html.Attributes.max "11"
        , on "input" targetValue (\val -> (Signal.message address (SetPitch val))) ] []
    ]

view address model =
  div [class "movie-player"] [
    (voiceSelect address hardcodedVoices)
    , (rateSelect address model.rate)
    , (pitchSelect address model.pitch)
  ]


--Signals and mailoboxes

changeVoice : SettingsModel -> Effects Action
changeVoice model =
  Signal.send voiceMailbox.address model
    |> Effects.task
    |> Effects.map (\_ -> NoOp)

voiceMailbox: Signal.Mailbox SettingsModel
voiceMailbox =
    Signal.mailbox initialModel

hardcodedVoices = [ "Alex"
          , "Agnes"
          , "Albert"
          , "Bad News"
          , "Bahh"
          , "Bells"
          , "Boing"
          , "Bruce"
          , "Bubbles"
          , "Cellos"
          , "Deranged"
          , "Fred"
          , "Good News"
          , "Hysterical"
          , "Junior"
          , "Kathy"
          , "Pipe Organ"
          , "Princess"
          , "Ralph"
          , "Samantha"
          , "Trinoids"
          , "Vicki"
          , "Victoria"
          , "Whisper"
          , "Zarvox"
          , "Google US English"
          ]


(=>) = (,)

rangeStyle: Attribute
rangeStyle =
  style [ "width" => "60%"

  ]
