module Main where

import Signal exposing (forwardTo)
import StartApp
import Effects
import Task

import Html.Attributes exposing (class)
import Html exposing (div, span, strong, text, h1)

import Global
import Counter
import CounterPair
import CounterList

import Ui.App

type alias Model =
  { app : Ui.App.Model
  , counter1 : Counter.Model Global.Action
  , counter2 : Counter.Model Global.Action
  , counterPair : CounterPair.Model Global.Action
  , counterList : CounterList.Model Global.Action
  , counter : Int
  }

type Action
  = Global Global.Action
  | Counter1 Counter.Action
  | Counter2 Counter.Action
  | CounterPair CounterPair.Action
  | CounterList CounterList.Action
  | App Ui.App.Action

global : Signal.Mailbox Global.Action
global =
  Signal.mailbox Global.Increment

init : Model
init =
  { counter1 = Counter.init global.address Global.Increment
  , counter2 = Counter.init global.address Global.Decrement
  , counterPair = CounterPair.init global.address Global.Increment
  , counterList = CounterList.init global.address Global.Increment
  , app = Ui.App.init "Counters"
  , counter = 0
  }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    App act ->
      let
        (app, effect) = Ui.App.update act model.app
      in
        ({ model | app = app }, Effects.map App effect)

    Counter1 act ->
      ({ model | counter1 = Counter.update act model.counter1 }, Effects.none)

    Counter2 act ->
      ({ model | counter2 = Counter.update act model.counter2 }, Effects.none)

    CounterPair act ->
      ({ model | counterPair = CounterPair.update act model.counterPair }, Effects.none)

    CounterList act ->
      ({ model | counterList = CounterList.update act model.counterList }, Effects.none)

    Global act ->
      case act of
        Global.Decrement ->
          let
            by = if (counterCount model) > 10 then 2 else 1
          in
            ({ model | counter = model.counter - by }, Effects.none)
        Global.Increment ->
          let
            by = if (counterCount model) > 10 then 2 else 1
          in
            ({ model | counter = model.counter + by }, Effects.none)

counterCount model =
  (Counter.counterCount model.counter1) +
    (CounterPair.counterCount model.counterPair) +
      (CounterList.counterCount model.counterList)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Ui.App.view (forwardTo address App) model.app
    [ div []
      [ div []
        [ h1 [] [text "Global Counter"]
        , h1 [] [text (toString model.counter)]
        , h1 [] [text "Counter Count"]
        , h1 [] [text (toString (counterCount model))]
        ]
      , div [class "counters"]
        [ Counter.view (forwardTo address Counter1) model.counter1
        , Counter.view (forwardTo address Counter2) model.counter2
        , CounterPair.view (forwardTo address CounterPair) model.counterPair
        , CounterList.view (forwardTo address CounterList) model.counterList
        ]
      ]
    ]

app =
  StartApp.start { init = (init, Effects.none)
                 , update = update
                 , view = view
                 , inputs = [Signal.map Global global.signal]
                 }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
