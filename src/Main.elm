
import Effects exposing (Never)
import RandomGif exposing (..)
import StartApp
import Task
import Time


--Inbound
port speakEnd : Signal (Int)

app =
  StartApp.start
    { init = init "CATS"
    , update = update
    , view = view
    , inputs = [Signal.map (always RandomGif.RequestMore) speakEnd]
    }


main =
  app.html

--Outbound

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port second : Signal Time.Time
port second =
    Time.every Time.second





