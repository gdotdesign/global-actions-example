module Counter where

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, span, text, button)

import Global

type alias Model =
  { counter: Int
  , globalAddress : Signal.Address Global.Action
  }

type Action
  = Increment
  | Decrement

init : Signal.Address Global.Action -> Model
init globalAddress =
  { counter = 0
  , globalAddress = globalAddress
  }

counterCount : Model -> Int
counterCount model =
  1

update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      { model | counter = model.counter + 1 }

    Decrement ->
      { model | counter = model.counter - 1 }

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [class "counter"]
    [ span [] [text (toString model.counter)]
    , button [onClick address Increment] [text "increment"]
    , button [onClick address Decrement] [text "decrement"]
    , button [onClick model.globalAddress Global.Increment] [text "global increment"]
    ]
