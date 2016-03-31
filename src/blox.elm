module Blox where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp

import Time
import Signal
import Random
import Mouse

-- GRAPHICS
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

-- TYPES


getSeed ms =
    Random.initialSeed (round ms)

getRandom seed =
    Random.generate (Random.int 0 100) seed

addTimes =
    Signal.foldp (::) [] (Time.every Time.second)

getP number =
    p [] [Html.text (toString number)]

getSpans randomNumber =
    span [] [Html.text randomNumber]

view ms =
    getSeed ms
    |> getRandom
    |> fst
    |> toString
    |> getSpans

main =
    Signal.map view (Time.every Time.second)




