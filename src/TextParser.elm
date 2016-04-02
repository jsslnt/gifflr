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
import String

import StopWords exposing (words)


splitSentence : String -> List String
splitSentence sentence =
    Regex.split Regex.All (Regex.regex "[ ]") sentence


constructSearchTermFromSentence : String -> String
constructSearchTermFromSentence sentence =
    let
        joinWithSpaces a b =
            a ++ " " ++ b
    in
        splitSentence sentence
        |> filterStopWords
        |> eliminateEmpties
        |> List.foldr joinWithSpaces ""


splitTextBlock : String -> List String
splitTextBlock inputText =
    eliminateEmpties (Regex.split Regex.All (Regex.regex "[.?!]") inputText)

eliminateEmpties : List String -> List String
eliminateEmpties text =
    let
        checkEmpty word =
            not (String.isEmpty word)
    in
        List.filter checkEmpty text


filterStopWords : List String -> List String
filterStopWords wordList =
    List.filter findInStopWords wordList


findInStopWords : String -> Bool
findInStopWords word =
    let
        stringsEqual a b =
            a /= b
    in
        List.all (stringsEqual word) StopWords.words
