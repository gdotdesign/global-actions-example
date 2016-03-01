module Counter where

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, span, text, button)

type alias Model a =
  { counter: Int
  , clickAddress : Signal.Address a
  , clickAction : a
  }

type Action
  = Increment
  | Decrement

init : Signal.Address a -> a -> Model a
init clickAddress clickAction =
  { counter = 0
  , clickAddress = clickAddress
  , clickAction = clickAction
  }

counterCount : Model a -> Int
counterCount model =
  1

update : Action -> Model a -> Model a
update action model =
  case action of
    Increment ->
      { model | counter = model.counter + 1 }

    Decrement ->
      { model | counter = model.counter - 1 }

view : Signal.Address Action -> Model a -> Html.Html
view address model =
  div [class "counter"]
    [ span [] [text (toString model.counter)]
    , button [onClick address Increment] [text "increment"]
    , button [onClick address Decrement] [text "decrement"]
    , button [onClick model.clickAddress model.clickAction] [text ("global " ++ toString model.clickAction)]
    ]
