module Main exposing (..)

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
    , color : Color
    , keyboardState : Keyboard.Extra.State
    , clock : Time
    , animation : Animation
    , animations : List (Time -> Animation)
    }


type alias Point =
    ( Int, Int )


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake
    | ChangeColor Color


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        keyboardState =
            Keyboard.Extra.initialState
    in
        ( { points = [ ( 0, 0 ) ]
          , x = 0
          , y = 0
          , keyboardState = keyboardState
          , color = red
          , clock = 0
          , animation = static 0
          , animations = []
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    let
        angle =
            animate model.clock model.animation
    in
        div []
            [ collage 800
                600
                [ rotate (degrees angle) (drawLine model.points model.color) ]
                |> Element.toHtml
            , shakeButton
            , div []
                [ Html.text "Line Color: "
                , colorButton red "Red"
                , colorButton blue "Blue"
                , colorButton green "Green"
                ]
            ]


drawLine : List Point -> Color -> Form
drawLine points color =
    let
        intsToFloats : ( Int, Int ) -> ( Float, Float )
        intsToFloats ( x, y ) =
            ( toFloat x, toFloat y )

        shape =
            path (List.map intsToFloats points)
    in
        shape
            |> traced (solid color)


colorButton : Color -> String -> Html Msg
colorButton color name =
    Html.button [ onClick (ChangeColor color) ] [ Html.text name ]


shakeButton : Html Msg
shakeButton =
    Html.button [ onClick Shake ] [ Html.text "Shake it good!" ]


shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 40
        |> duration (200 * Time.millisecond)


shakeAnimation_ : Time -> Animation
shakeAnimation_ t =
    animation t
        |> from 40
        |> to -20
        |> duration (200 * Time.millisecond)


shakeAnimation__ : Time -> Animation
shakeAnimation__ t =
    animation t
        |> from -20
        |> to 10
        |> duration (200 * Time.millisecond)


shakeAnimation___ : Time -> Animation
shakeAnimation___ t =
    animation t
        |> from 10
        |> to 0
        |> duration (200 * Time.millisecond)


animations : List (Time -> Animation)
animations =
    [ shakeAnimation
    , shakeAnimation_
    , shakeAnimation__
    , shakeAnimation___
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.diffs Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardExtraMsg keyMsg ->
            let
                keyboardState =
                    Keyboard.Extra.update keyMsg model.keyboardState
            in
                { model | keyboardState = keyboardState } ! []

        Tick dt ->
            let
                { x, y } =
                    Keyboard.Extra.arrows model.keyboardState

                newX =
                    model.x + x

                newY =
                    model.y + y

                newClock =
                    model.clock + dt

                ( newPoints, newAnimation, newAnimations ) =
                    case (isDone model.clock model.animation) of
                        True ->
                            let
                                nextAnimation =
                                    case List.head model.animations of
                                        Just animation ->
                                            animation model.clock

                                        Nothing ->
                                            static 0

                                nextAnimations =
                                    (List.tail model.animations) |> Maybe.withDefault ([])

                                justFinished =
                                    nextAnimation
                                        == (static 0)
                                        && not (model.animation == (static 0))

                                nextPoints =
                                    case justFinished of
                                        True ->
                                            []

                                        False ->
                                            model.points
                            in
                                ( nextPoints, nextAnimation, nextAnimations )

                        False ->
                            ( model.points, model.animation, model.animations )

                newPoints_ =
                    case ( x, y ) of
                        ( 0, 0 ) ->
                            newPoints

                        _ ->
                            ( newX, newY ) :: newPoints

                model_ =
                    { model
                        | points = newPoints_
                        , clock = newClock
                        , animation = newAnimation
                        , animations = newAnimations
                    }
            in
                case ( x, y ) of
                    ( 0, 0 ) ->
                        model_ ! []

                    _ ->
                        { model_ | x = newX, y = newY } ! []

        Shake ->
            { model
                | animations = animations
            }
                ! []

        ChangeColor color ->
            { model | color = color } ! []


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
