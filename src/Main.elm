
import Effects exposing (Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( on, targetValue, onClick)
import TextParser exposing (..)
import StartApp
import Task
import Time


--Inbound
port speakEnd : Signal (Int)

app : StartApp.App Model
app =
  StartApp.start
    { init = TextParser.init
    , update = TextParser.update
    , view = TextParser.view
    , inputs = [Signal.map TextParser.PlaySentence speakEnd]
    }

main : Signal.Signal Html.Html
main =
  app.html

--Outbound

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port speakSentence : Signal String
port speakSentence =
    spokenMailbox.signal





