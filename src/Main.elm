
import Effects exposing (Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( on, targetValue, onClick)
import StartApp
import Task
import Time

import Storyboard
import MoviePlayer exposing (spokenMailbox)
import VoiceSettings exposing (voiceMailbox, SettingsModel)


-- Model

type alias AppModel =
  { isPlaying: Bool
  , storyboardModel : Storyboard.Model
  , moviePlayerModel: MoviePlayer.Model
  }

initialModel : AppModel
initialModel =
  { isPlaying = False
  , storyboardModel = Storyboard.initialModel
  , moviePlayerModel = MoviePlayer.initialModel
  }

-- Action

type Action
  = NoOp
  | StoryboardAction Storyboard.Action
  | MoviePlayerAction MoviePlayer.Action


-- Update

update : Action -> AppModel -> ( AppModel, Effects.Effects Action )
update action model =
  case action of

    StoryboardAction childAction ->
      let
        (result, fx) = Storyboard.update childAction model.storyboardModel
        isPlaying = result.playRequested
        wasPlaying = model.isPlaying
      in
        ({ model | storyboardModel = result
                 , moviePlayerModel = MoviePlayer.createModel result.sentences
                 , isPlaying = isPlaying
         }
        , if isPlaying && not wasPlaying then
            Effects.batch
              [ Effects.map StoryboardAction fx,
                Effects.map MoviePlayerAction MoviePlayer.startMovie
              ]
          else
            Effects.map StoryboardAction fx
      )

    MoviePlayerAction childAction ->
      let
        (result, fx) = MoviePlayer.update childAction model.moviePlayerModel
      in
        ({ model | moviePlayerModel = result }
        , Effects.map MoviePlayerAction fx
        )
    NoOp ->
      (model, Effects.none)

-- View

view : Signal.Address Action -> AppModel -> Html
view address model =
  Html.div
    []
    [
        MoviePlayer.view (Signal.forwardTo address MoviePlayerAction) model.moviePlayerModel
        , Storyboard.view (Signal.forwardTo address StoryboardAction) model.storyboardModel
    ]

--Inbound ports

port speakEnd : Signal (Int)

--Outbound ports

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port speakSentence : Signal String
port speakSentence =
  spokenMailbox.signal

port changeVoice : Signal SettingsModel
port changeVoice =
    voiceMailbox.signal

-- Start app

init : (AppModel, Effects.Effects Action)
init = (initialModel, Effects.none)

inputs =
  [ Signal.map MoviePlayerAction (MoviePlayer.createPlayInput speakEnd)
  ]

app : StartApp.App AppModel
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }

main : Signal.Signal Html.Html
main =
  app.html
