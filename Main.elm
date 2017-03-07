import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)

type alias Model =
  { points : List Point
  , x : Int
  , y : Int
  }

type alias Point = (Int, Int)

type Msg = KeyUp Keyboard.KeyCode

initialModel : Model
initialModel =
  { points = [(0, 0)]
  , x = 0
  , y = 0
  }

view : Model -> Html Msg
view model =
  collage 800 600
    [ drawLine model.points ]
    |> Element.toHtml

drawLine : List Point -> Form
drawLine points =
  let
    intsToFloats : (Int, Int) -> (Float, Float)
    intsToFloats (x, y) =
      (toFloat x, toFloat y)

    shape = path (List.map intsToFloats points)
  in
    shape
    |> traced (solid red)


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.ups KeyUp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyUp keyCode ->
      (keyUp keyCode model, Cmd.none)

keyUp : Keyboard.KeyCode -> Model -> Model
keyUp keyCode model =
  case keyCode of
    38 -> -- up
      { model | y = model.y + 1, points = (model.x, model.y + 1) :: model.points }
    40 -> -- down
      { model | y = model.y - 1, points = (model.x, model.y - 1) :: model.points }
    39 -> -- right
      { model | x = model.x + 1, points = (model.x + 1, model.y) :: model.points }
    37 -> -- left
      { model | x = model.x - 1, points = (model.x - 1, model.y) :: model.points }
    _ -> model

main : Program Never Model Msg
main =
  Html.program
    { init = initialModel ! []
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
