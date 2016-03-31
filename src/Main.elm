
import Effects exposing (Never)
import RandomGif exposing (init, update, view)
import StartApp
import Task

app =
  StartApp.start
    { init = init "CATS"
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks





