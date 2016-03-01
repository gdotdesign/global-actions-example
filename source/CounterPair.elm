module CounterPair where

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, text, button)
import Signal exposing (forwardTo)

import Counter

type alias Model a =
  { counter1: Counter.Model a
  , counter2: Counter.Model a
  }

type Action
  = Counter1 Counter.Action
  | Counter2 Counter.Action

init : Signal.Address a -> a -> Model a
init clickAddress clickAction =
  { counter1 = Counter.init clickAddress clickAction
  , counter2 = Counter.init clickAddress clickAction
  }

counterCount : Model a -> Int
counterCount model =
  2

update : Action -> Model a -> Model a
update action model =
  case action of
    Counter1 act ->
      { model | counter1 = Counter.update act model.counter1 }
    Counter2 act ->
      { model | counter2 = Counter.update act model.counter2 }

view : Signal.Address Action -> Model a -> Html.Html
view address model =
  div [class "counter-pair"]
    [ div []
      [ Counter.view (forwardTo address Counter1) model.counter1
      , Counter.view (forwardTo address Counter2) model.counter2
      ]
    ]
