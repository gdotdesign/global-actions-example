module CounterPair where

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, text, button)
import Signal exposing (forwardTo)

import Counter

import Global

type alias Model =
  { counter1: Counter.Model
  , counter2: Counter.Model
  , globalAddress : Signal.Address Global.Action
  }

type Action
  = Counter1 Counter.Action
  | Counter2 Counter.Action

init : Signal.Address Global.Action -> Model
init globalAddress =
  { counter1 = Counter.init globalAddress
  , counter2 = Counter.init globalAddress
  , globalAddress = globalAddress
  }

counterCount : Model -> Int
counterCount model =
  2

update : Action -> Model -> Model
update action model =
  case action of
    Counter1 act ->
      { model | counter1 = Counter.update act model.counter1 }
    Counter2 act ->
      { model | counter2 = Counter.update act model.counter2 }

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [class "counter-pair"]
    [ div []
      [ Counter.view (forwardTo address Counter1) model.counter1
      , Counter.view (forwardTo address Counter2) model.counter2
      ]
    ]
