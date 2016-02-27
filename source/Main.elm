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
  , comp1 : Counter.Model
  , comp2 : CounterPair.Model
  , comp3 : CounterList.Model
  , counter : Int
  }

type Action
  = Global Global.Action
  | Counter Counter.Action
  | CounterPair CounterPair.Action
  | CounterList CounterList.Action
  | App Ui.App.Action

global : Signal.Mailbox Global.Action
global =
  Signal.mailbox Global.Increment

init : Model
init =
  { comp1 = Counter.init global.address
  , comp2 = CounterPair.init global.address
  , comp3 = CounterList.init global.address
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

    Counter act ->
      ({ model | comp1 = Counter.update act model.comp1 }, Effects.none)

    CounterPair act ->
      ({ model | comp2 = CounterPair.update act model.comp2 }, Effects.none)

    CounterList act ->
      ({ model | comp3 = CounterList.update act model.comp3 }, Effects.none)

    Global act ->
      case act of
        Global.Increment ->
          let
            by = if (counterCount model) > 10 then 2 else 1
          in
            ({ model | counter = model.counter + by }, Effects.none)

counterCount model =
  (Counter.counterCount model.comp1) +
    (CounterPair.counterCount model.comp2) +
      (CounterList.counterCount model.comp3)

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
        [ Counter.view (forwardTo address Counter) model.comp1
        , CounterPair.view (forwardTo address CounterPair) model.comp2
        , CounterList.view (forwardTo address CounterList) model.comp3
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
