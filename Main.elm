import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Keyboard.Extra
import Time exposing (Time, second)
import Html exposing (..)

type alias Model =
  { points : List Point
  , x : Int
  , y : Int
  , keyboardState : Keyboard.Extra.State
  }

type alias Point = (Int, Int)

type Msg
  = KeyboardExtraMsg Keyboard.Extra.Msg
  | Tick Time

initialModel : ( Model, Cmd Msg )
initialModel =
  let
      keyboardState = Keyboard.Extra.initialState
  in
    ( { points = [(0, 0)]
      , x = 0
      , y = 0
      , keyboardState= keyboardState
      }
    , Cmd.none
    )

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
  Sub.batch
  [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
  , Time.every (1/30 * second) Tick
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyboardExtraMsg keyMsg ->
      let
        keyboardState = Keyboard.Extra.update keyMsg model.keyboardState
      in
       { model | keyboardState = keyboardState } ! []
    Tick _ ->
      let
        { x, y } = Keyboard.Extra.arrows model.keyboardState
        newX = model.x + x
        newY = model.y + y
      in
        case (x, y) of
          (0, 0) ->
            model ! []
          _ ->
            { model | x = newX, y = newY, points = (newX, newY) :: model.points } ! []

main : Program Never Model Msg
main =
  Html.program
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
