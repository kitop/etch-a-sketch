import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Keyboard.Extra
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)

type alias Model =
  { points : List Point
  , x : Int
  , y : Int
  , keyboardState : Keyboard.Extra.State
  , clock : Time
  , animation : Animation
  }

type alias Point = (Int, Int)

type Msg
  = KeyboardExtraMsg Keyboard.Extra.Msg
  | Tick Time
  | Shake

initialModel : ( Model, Cmd Msg )
initialModel =
  let
      keyboardState = Keyboard.Extra.initialState
  in
    ( { points = [(0, 0)]
      , x = 0
      , y = 0
      , keyboardState = keyboardState
      , clock = 0
      , animation = static 0
      }
    , Cmd.none
    )

view : Model -> Html Msg
view model =
  let
      angle = animate model.clock model.animation
  in
    div []
      [ collage 800 600
          [ rotate (degrees angle) (drawLine model.points) ]
          |> Element.toHtml
      , shakeButton
      ]

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

shakeButton : Html Msg
shakeButton =
  Html.button [ onClick Shake ] [ Html.text "Shake it good!" ]

shakeAnimation : Time -> Animation
shakeAnimation t =
  animation t
    |> from 0
    |> to 360
    |> duration (500 * Time.millisecond)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
  , AnimationFrame.diffs Tick
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyboardExtraMsg keyMsg ->
      let
        keyboardState = Keyboard.Extra.update keyMsg model.keyboardState
      in
       { model | keyboardState = keyboardState } ! []
    Tick dt ->
      let
        { x, y } = Keyboard.Extra.arrows model.keyboardState
        newX = model.x + x
        newY = model.y + y
        newClock = model.clock + dt
        (newPoints, newAnimation) =
          case (model.animation == (static 0)) of
            True ->
              (model.points, model.animation)
            False ->
              case (isDone model.clock model.animation) of
                True -> ([], (static 0))
                False -> (model.points, model.animation)

        newPoints_ =
          case (x, y) of
            (0, 0) -> newPoints
            _ -> (newX, newY) :: newPoints

        model_ =
          { model
          | points = newPoints_
          , clock = newClock
          , animation = newAnimation
          }
      in
        case (x, y) of
          (0, 0) ->
            model_ ! []
          _ ->
            { model_ | x = newX, y = newY } ! []
    Shake ->
      { model
      | animation = shakeAnimation model.clock
      } ! []

main : Program Never Model Msg
main =
  Html.program
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
