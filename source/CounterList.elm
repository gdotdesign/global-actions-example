module CounterList where

import Array exposing (Array)
import Array.Extra
import Signal exposing (forwardTo)

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, span, strong, text, button)

import CounterPair

type alias Model a =
  { counters : Array (CounterPair.Model a)
  , clickAddress : Signal.Address a
  , clickAction : a
  }

type Action
  = Counter Int CounterPair.Action
  | Add
  | Remove Int

init : Signal.Address a -> a -> Model a
init clickAddress clickAction =
  { counters = Array.fromList []
  , clickAddress = clickAddress
  , clickAction = clickAction
  }

counterCount : Model a -> Int
counterCount model =
  Array.map (\counters -> CounterPair.counterCount counters) model.counters
  |> Array.foldl (+) 0

updateCounter : Int -> CounterPair.Action -> Array (CounterPair.Model a) -> Array (CounterPair.Model a)
updateCounter index action counters =
  let
    updatedCounter counterIndex counter =
      if counterIndex == index then
        CounterPair.update action counter
      else
        counter
  in
    Array.indexedMap updatedCounter counters

update : Action -> Model a -> Model a
update action model =
  case action of
    Add ->
      { model | counters = Array.push (CounterPair.init model.clickAddress model.clickAction) model.counters}
    Remove index ->
      { model | counters = Array.Extra.removeAt index model.counters}
    Counter index act ->
      { model | counters = updateCounter index act model.counters }


view : Signal.Address Action -> Model a -> Html.Html
view address model =
  let
    counter index counter =
      div []
        [ CounterPair.view (forwardTo address (Counter index)) counter
        , button [onClick address (Remove index)] [text "-"]
        ]

    counters =
      Array.indexedMap counter model.counters
      |> Array.toList
  in
    div [class "counter-list"]
      [ button [onClick address Add] [text "Add Counter"]
      , div [] counters
      ]
