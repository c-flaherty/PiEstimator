module Pi exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Html exposing (Html, text)
import Html.Attributes
import Random exposing (Generator)
import Time
import Debug
import Collage exposing (circle, rectangle, filled, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Collage.Text
import Color
import FormatNumber
import FormatNumber.Locales

import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
----------------------------------------------------------------------

{-
Notes:
  1) I used a color palette generator: https://www.color-hex.com/color-palette/44370.
  2) I am using cuducos/elm-format-number to format the Pi estimation to a fixed number of digits after the decimal.

------------------
Estimating Pi:

P(point in circle) = (Area of the circle) / (Area of the entire square) = pi^1^2 / 4 = pi/4

P~(point in circle) = (num of points in circle)/(num of total points)

pi is approximately 4*(num of points in circle)/(num of total points)

--------------

Parts of a UI:
1) View - describes visual aspects of UI 
2) Model - stores all the data 
3) Controller - updates data 

Model --> View
Controller --> Model
View -> Controller

Two sources of input:
1) Model should "subscribe" to a clock that updates every 10 milliseconds saying "Hey, 10 seconds have passed."
2) User-input (can happen at any time)

What data do we need to store in the model?
-> minimum amount of data needed to represent what is going on
-> to save computation, we might want to "cache" some "derived" values

What kind of events do can happen + we care about?

Finally, we need some code to produce the "view".


Summary:
- define a model schema 
- define a set of possible events 
- write a "subscription" function to monitor the clock
- write an "update" function for those events and model schema, should output an updated model
- write a "view" function that can produce a view from a model

-}


main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Point = { x:Float, y:Float }

type alias Model =
  { hits : List Point
  , misses : List Point
  , hitCount : Int
  , missCount : Int
  , stop : Bool
  }

type Msg = Tick | RandomPoint Point | Restart | Stop

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

initModel : Model
initModel =
  {
    hits= [],
    misses= [],
    hitCount= 0,
    missCount= 0,
    stop = False
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 (\_ -> Tick)


pointGenerator : Generator Point
pointGenerator =
  Random.pair (Random.float -1 1) (Random.float -1 1)
  |> Random.map (\(x_,y_) -> {x=x_, y=y_})

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Tick -> 
      let 
        newPoint = Random.generate RandomPoint pointGenerator
      in
        (model, newPoint)
    (RandomPoint point) ->
      if model.stop == False then
        let 
          radius = sqrt (point.x ^ 2 + point.y ^ 2)

          newModel = 
            if radius <= 1  then
              { hits = point::model.hits
              , misses= model.misses
              , hitCount= model.hitCount + 1
              , missCount= model.missCount
              , stop = False
              }
            else 
              { hits = model.hits
              , misses= point::model.misses
              , hitCount= model.hitCount
              , missCount= model.missCount + 1
              , stop = False
              }
        in
          (newModel, Cmd.none)
      else 
        (model, Cmd.none)
    Restart -> 
      (initModel, Cmd.none)
    Stop -> 
      ({model | stop=True }, Cmd.none)


view : Model -> Html Msg
view model =
  let 
    rect = rectangle 400 400
      |> Collage.outlined Collage.invisible

    createCirc : Color.Color -> (Float, Point) -> Collage.Collage msg
    createCirc color (opac, p) = 
      Collage.circle 5 
        |> filled (uniform color)
        |> Collage.opacity opac
        |> Collage.shift (200*p.x, 200*p.y)

    zipGradient : List (Point) -> List (Float, Point)
    zipGradient pts =
      let 
        listLength = toFloat (List.length pts)
      in 
        List.reverse pts
        |> List.indexedMap (\idx -> \pt -> ((toFloat idx)/listLength, pt))
        |> List.reverse

    hit_circs = 
      zipGradient model.hits
      |> List.map (createCirc (Color.rgb255 70 237 200))

    miss_circs = 
      zipGradient model.misses
      |> List.map (createCirc (Color.rgb255 253 242 137))

    circs = hit_circs ++ miss_circs

    foreground = Collage.group circs
    background = rect 

    img = Collage.Layout.impose foreground background

    pi = 4*(toFloat  model.hitCount)/(toFloat (model.missCount + model.hitCount))
    formatted_pi = FormatNumber.format (FormatNumber.Locales.Locale (FormatNumber.Locales.Exact 8) "," "." "−" "" "" "" "" "") pi

    text = Collage.Text.fromString ("Pi: " ++ formatted_pi)
      |> Collage.Text.size Collage.Text.huge
      |> Collage.Text.typeface (Collage.Text.Font "Courier New")
      |> Collage.Text.color (Color.rgb255 55 77 124)
      |> Collage.Text.weight Collage.Text.Bold
      |> Collage.rendered

    hspace = Collage.Layout.spacer 0 50
    all = Collage.Layout.vertical <| [img, hspace, text]
  in 
    Grid.container [
      Html.Attributes.style "height" "100vh",
      Html.Attributes.style "display" "grid",
      Html.Attributes.style "place-items" "center"
    ]
      [
        CDN.stylesheet,
        all
            |> svg,
        ButtonGroup.buttonGroup
          []
          [
            ButtonGroup.button [Button.primary, Button.onClick Restart] [Html.text "Restart"],
            ButtonGroup.button [Button.primary, Button.onClick Stop] [Html.text "Stop"]
          ]
      ]