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

import StopWords exposing (words)

splitSentences : String -> List String
splitSentences inputText =
    Regex.split Regex.All (Regex.regex "[.?!]") inputText
