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

import StopWords exposing (words)

splitTextBlock : String -> List String
splitTextBlock inputText =
    Regex.split Regex.All (Regex.regex "[.?!]") inputText

splitSentence : String -> List String
splitSentence sentence =
    Regex.split Regex.All (Regex.regex "[ ]") sentence

filterStopWords : List String -> List String
filterStopWords wordList =
    List.filter findInStopWords wordList

joinWithSpaces : String -> String -> String
joinWithSpaces a b =
  a ++ " " ++ b

constructSearchTermFromSentence : String -> String
constructSearchTermFromSentence sentence =
  List.foldr joinWithSpaces "" (filterStopWords (splitSentence sentence))

findInStopWords : String -> Bool
findInStopWords word =
    let
        foundInStopWords = False
    in
        List.all (stringsEqual word) StopWords.words

stringsEqual : String -> String -> Bool
stringsEqual a b =
    a /= b
