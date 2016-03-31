module TextParsing where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( on, targetValue, onClick)
import StartApp.Simple as StartApp
import StopWords exposing (words)
import Regex exposing (split)

type Action = UpdateText String | GenerateSentences

type alias Model = 
    {
        inputText : String,
        sentences : List String
    }

initialModel : Model
initialModel = 
    {
        inputText = "",
        sentences = []
    }
 

update action model = 
    case action of 
        UpdateText newText -> 
            { model | inputText = newText }
        GenerateSentences ->
            { model | sentences = splitSentences model.inputText}

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
        , type' "submit" 
        , onClick address GenerateSentences ] [text "submit story"]


view address model = 
    body [] 
    [ header
        , div [] (List.map story model.sentences)
        , input [placeholder "placeholder"
            , type' "text"
            , on "input" targetValue (\val -> (Signal.message address (UpdateText val))) ] []
        , submitButton address ]


main = 
    StartApp.start { model = initialModel, 
        view = view,
        update = update }
    --pageStructure

