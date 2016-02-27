module CounterList where

import Array exposing (Array)
import Array.Extra
import Signal exposing (forwardTo)

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, span, strong, text, button)

import CounterPair
import Global

type alias Model =
  { counters : Array CounterPair.Model
  , globalAddress : Signal.Address Global.Action
  }

type Action
  = Counter Int CounterPair.Action
  | Add
  | Remove Int

init : Signal.Address Global.Action -> Model
init globalAddress =
  { counters = Array.fromList []
  , globalAddress = globalAddress
  }

counterCount : Model -> Int
counterCount model =
  Array.map (\counters -> CounterPair.counterCount counters) model.counters
  |> Array.foldl (+) 0

updateCounter : Int -> CounterPair.Action -> Array CounterPair.Model -> Array CounterPair.Model
updateCounter index action counters =
  let
    updatedCounter counterIndex counter =
      if counterIndex == index then
        CounterPair.update action counter
      else
        counter
  in
    Array.indexedMap updatedCounter counters

update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      { model | counters = Array.push (CounterPair.init model.globalAddress) model.counters}
    Remove index ->
      { model | counters = Array.Extra.removeAt index model.counters}
    Counter index act ->
      { model | counters = updateCounter index act model.counters }


view : Signal.Address Action -> Model -> Html.Html
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
